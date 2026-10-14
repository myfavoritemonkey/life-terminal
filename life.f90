module life_rules
  implicit none
contains
  subroutine step(board, next)
    logical, intent(in) :: board(:, :)
    logical, intent(out) :: next(:, :)
    integer :: x, y, left, right, above, below, neighbors, width, height

    width = size(board, 1)
    height = size(board, 2)
    do y = 1, height
      above = y - 1
      below = y + 1
      if (above == 0) above = height
      if (below > height) below = 1
      do x = 1, width
        left = x - 1
        right = x + 1
        if (left == 0) left = width
        if (right > width) right = 1
        neighbors = 0
        if (board(left, above)) neighbors = neighbors + 1
        if (board(x, above)) neighbors = neighbors + 1
        if (board(right, above)) neighbors = neighbors + 1
        if (board(left, y)) neighbors = neighbors + 1
        if (board(right, y)) neighbors = neighbors + 1
        if (board(left, below)) neighbors = neighbors + 1
        if (board(x, below)) neighbors = neighbors + 1
        if (board(right, below)) neighbors = neighbors + 1
        next(x, y) = neighbors == 3 .or. (board(x, y) .and. neighbors == 2)
      end do
    end do
  end subroutine step

  subroutine step_bounded(board, next)
    logical, intent(in) :: board(:, :)
    logical, intent(out) :: next(:, :)
    integer :: x, y, dx, dy, nx, ny, neighbors, width, height

    width = size(board, 1)
    height = size(board, 2)
    do y = 1, height
      do x = 1, width
        neighbors = 0
        do dy = -1, 1
          ny = y + dy
          if (ny < 1 .or. ny > height) cycle
          do dx = -1, 1
            if (dx == 0 .and. dy == 0) cycle
            nx = x + dx
            if (nx < 1 .or. nx > width) cycle
            if (board(nx, ny)) neighbors = neighbors + 1
          end do
        end do
        next(x, y) = neighbors == 3 .or. (board(x, y) .and. neighbors == 2)
      end do
    end do
  end subroutine step_bounded
end module life_rules

program game_of_life
  use iso_c_binding, only: c_int, c_long, c_short, c_size_t, c_char
  use iso_fortran_env, only: int64, output_unit, error_unit
  use life_rules
  implicit none

  type, bind(c) :: window_size
    integer(c_short) :: rows, columns, x_pixels, y_pixels
  end type

  interface
    function isatty(fd) bind(c, name='isatty') result(value)
      import c_int
      integer(c_int), value :: fd
      integer(c_int) :: value
    end function
    function c_read(fd, buffer, nbytes) bind(c, name='read') result(nread)
      import c_int, c_long, c_size_t, c_char
      integer(c_int), value :: fd
      character(c_char) :: buffer(*)
      integer(c_size_t), value :: nbytes
      integer(c_long) :: nread
    end function
    function usleep(microseconds) bind(c, name='usleep') result(status)
      import c_int
      integer(c_int), value :: microseconds
      integer(c_int) :: status
    end function
    function ioctl(fd, request, window) bind(c, name='ioctl') result(status)
      import c_int, c_long, window_size
      integer(c_int), value :: fd
      integer(c_long), value :: request
      type(window_size) :: window
      integer(c_int) :: status
    end function
  end interface

  integer :: width, height, speed, generation, steps_this_second, measured_speed
  integer :: x, y, count_args, ios, sleep_status, batch
  integer(int64) :: ticks, tick_rate, last_tick, stats_tick, start_tick
  real(8) :: elapsed, backlog, draw_elapsed, now
  logical :: running, redraw, quit, changed, random_mode, show_trails
  logical :: menu_open
  integer :: scene_id
  logical, allocatable :: board(:, :), next(:, :), initial(:, :), before_batch(:, :)
  integer, allocatable :: age(:, :)
  real(8), allocatable :: last_death(:, :)
  real, allocatable :: random_values(:, :)
  type(window_size) :: terminal
  character(c_char) :: key(1)
  character(32) :: argument
  character(128) :: status_line
  character(96) :: scene_title, scene_description, menu_message
  integer(c_long) :: bytes_read
  character(1), parameter :: esc = achar(27)
  ! Pattern data: https://www.brianlane.com/git/sdl2-life/file/examples/glider-gun.rle.html
  character(*), parameter :: gosper_rle = &
    '24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$'// &
    '2o8bo5bo3b2o$2o8bo3bob2o4bobo$10bo5bo7bo$11bo3bo$12b2o!'

  width = 100
  height = 30
  if (isatty(1_c_int) /= 0) then
    if (ioctl(1_c_int, 21523_c_long, terminal) == 0) then
      if (terminal%columns >= 5) width = min(200, int(terminal%columns) - 2)
      if (terminal%rows >= 5) height = min(80, int(terminal%rows) - 2)
    end if
  end if
  speed = 10
  count_args = command_argument_count()
  x = 1
  do while (x <= count_args)
    call get_command_argument(x, argument)
    select case (trim(argument))
    case ('--help', '-h')
      print '(a)', 'Usage: life [--width N] [--height N] [--speed 10] [--self-test]'
      print '(a)', 'Keys: M scenes, 0-5 select, Space pause, R restart, N new, T trails, +/- speed, Q quit.'
      stop
    case ('--self-test')
      call self_test()
      stop
    case ('--width', '--height', '--speed')
      if (x == count_args) call usage_error('Missing value for '//trim(argument))
      if (trim(argument) == '--width') then
        call read_number(width)
      else if (trim(argument) == '--height') then
        call read_number(height)
      else
        call read_number(speed)
      end if
    case default
      call usage_error('Unknown argument: '//trim(argument))
    end select
    x = x + 1
  end do
  if (width < 3 .or. width > 200 .or. height < 3 .or. height > 80) &
    call usage_error('Width must be 3..200 and height 3..80')
  if (speed < 1 .or. speed > 10000) call usage_error('Speed must be 1..10000')
  if (isatty(0_c_int) == 0) call usage_error('Run this game in a terminal')
  if (isatty(1_c_int) == 0) call usage_error('Run this game in a terminal')

  allocate(board(width, height), next(width, height), initial(width, height), before_batch(width, height), &
    random_values(width, height), age(width, height), last_death(width, height))
  call random_seed()
  call new_field()
  scene_id = 0
  scene_title = 'Random'
  scene_description = ''
  menu_message = ''
  menu_open = .true.
  random_mode = .true.
  show_trails = .true.
  running = .true.
  redraw = .true.
  quit = .false.
  backlog = 0.0d0
  draw_elapsed = 0.0d0
  steps_this_second = 0
  measured_speed = 0
  call system_clock(last_tick, tick_rate)
  start_tick = last_tick
  stats_tick = last_tick

  call execute_command_line('stty -echo -icanon -isig min 0 time 0')
  write(output_unit, '(a)', advance='no') esc//'[?1049h'//esc//'[?25l'//esc//'[2J'
  flush(output_unit)

  do while (.not. quit)
    do
      bytes_read = c_read(0_c_int, key, 1_c_size_t)
      if (bytes_read <= 0) exit
      changed = .true.
      if (key(1) == 'q' .or. key(1) == 'Q' .or. key(1) == achar(3)) then
        quit = .true.
      else if (menu_open) then
        select case (key(1))
        case ('0':'5')
          call load_scene(iachar(key(1)) - iachar('0'))
        case ('m', 'M', achar(27))
          menu_open = .false.
        case default
          changed = .false.
        end select
      else
        select case (key(1))
        case ('m', 'M')
          menu_open = .true.
          menu_message = ''
          backlog = 0.0d0
        case (' ')
          running = .not. running
          backlog = 0.0d0
        case ('r', 'R')
          board = initial
          age = merge(1, 0, board)
          last_death = -2.0d0
          generation = 0
          backlog = 0.0d0
          steps_this_second = 0
        case ('n', 'N')
          call load_scene(0)
        case ('c', 'C')
          random_mode = .not. random_mode
          if (scene_id > 0) then
            scene_id = 0
            initial = board
          end if
          backlog = 0.0d0
        case ('t', 'T')
          show_trails = .not. show_trails
        case ('+', '=')
          speed = min(10000, speed * 2)
          backlog = 0.0d0
        case ('-', '_')
          speed = max(1, speed / 2)
          backlog = 0.0d0
        case default
          changed = .false.
        end select
      end if
      if (changed) redraw = .true.
      if (quit) exit
    end do
    if (quit) exit

    if (redraw) then
      if (menu_open) then
        call draw_menu()
      else
        call draw()
      end if
      draw_elapsed = 0.0d0
      redraw = .false.
    end if

    call system_clock(ticks)
    elapsed = real(ticks - last_tick, 8) / real(tick_rate, 8)
    last_tick = ticks
    elapsed = min(max(elapsed, 0.0d0), 0.25d0)
    now = real(ticks - start_tick, 8) / real(tick_rate, 8)
    if (menu_open) then
      sleep_status = usleep(10000_c_int)
      cycle
    end if
    if (running) then
      backlog = min(backlog + elapsed * real(speed, 8), real(speed, 8) * 0.25d0)
      batch = 0
      if (backlog >= 1.0d0) before_batch = board
      do while (backlog >= 1.0d0 .and. batch < 64)
        if (random_mode) then
          call random_field(next)
        else if (scene_id > 0) then
          call step_bounded(board, next)
        else
          call step(board, next)
        end if
        board = next
        generation = generation + 1
        steps_this_second = steps_this_second + 1
        backlog = backlog - 1.0d0
        batch = batch + 1
      end do
      if (batch > 0) then
        do y = 1, height
          do x = 1, width
            if (board(x, y)) then
              if (before_batch(x, y)) then
                age(x, y) = min(age(x, y) + 1, 5)
              else
                age(x, y) = 1
              end if
            else
              if (before_batch(x, y)) last_death(x, y) = now
              age(x, y) = 0
            end if
          end do
        end do
      end if
    end if

    draw_elapsed = draw_elapsed + elapsed
    if (real(ticks - stats_tick, 8) / real(tick_rate, 8) >= 1.0d0) then
      measured_speed = steps_this_second
      steps_this_second = 0
      stats_tick = ticks
      redraw = .true.
    end if
    if (draw_elapsed >= 0.05d0) then
      call draw()
      draw_elapsed = 0.0d0
      redraw = .false.
    end if
    sleep_status = usleep(1000_c_int)
  end do

  write(output_unit, '(a)', advance='no') esc//'[?25h'//esc//'[0m'//esc//'[?1049l'
  flush(output_unit)
  call execute_command_line('stty sane')

contains
  subroutine read_number(value)
    integer, intent(out) :: value
    x = x + 1
    call get_command_argument(x, argument)
    read(argument, *, iostat=ios) value
    if (ios /= 0) call usage_error('Expected a whole number')
  end subroutine

  subroutine usage_error(message)
    character(*), intent(in) :: message
    write(error_unit, '(a)') trim(message)
    stop 1
  end subroutine

  subroutine new_field()
    call random_field(board)
    initial = board
    age = merge(1, 0, board)
    last_death = -2.0d0
    generation = 0
  end subroutine

  subroutine load_scene(id)
    integer, intent(in) :: id
    integer :: left, top

    if (id == 5 .and. (width < 40 .or. height < 14)) then
      menu_message = 'Gosper gun needs a field of at least 40 x 14.'
      menu_open = .true.
      return
    end if
    scene_id = id
    menu_message = ''
    menu_open = .false.
    running = .true.
    backlog = 0.0d0
    steps_this_second = 0
    measured_speed = 0
    if (id == 0) then
      scene_title = 'Random'
      scene_description = ''
      random_mode = .true.
      call new_field()
      return
    end if

    random_mode = .false.
    board = .false.
    select case (id)
    case (1)
      scene_title = 'Block'
      scene_description = 'Still life: the 2x2 block keeps its shape.'
      left = (width - 2) / 2 + 1
      top = (height - 2) / 2 + 1
      call place_rle(board, '2o$2o!', left, top)
    case (2)
      scene_title = 'Blinker'
      scene_description = 'Oscillator: changes shape every step, period 2.'
      left = (width - 3) / 2 + 1
      top = height / 2
      call place_rle(board, '3o!', left, top)
    case (3)
      scene_title = 'Glider'
      scene_description = 'Spaceship: moves diagonally down and right.'
      left = max(1, width / 4)
      top = max(1, height / 4)
      call place_rle(board, 'bob$2bo$3o!', left, top)
    case (4)
      scene_title = 'R-pentomino'
      scene_description = 'Five cells grow into a complex changing pattern.'
      left = (width - 3) / 2 + 1
      top = (height - 3) / 2 + 1
      call place_rle(board, 'b2o$2o$bo!', left, top)
    case (5)
      scene_title = 'Gosper glider gun'
      scene_description = 'Glider factory: one new glider every 30 steps.'
      call place_rle(board, gosper_rle, 3, 2)
    end select
    initial = board
    age = merge(1, 0, board)
    last_death = -2.0d0
    generation = 0
  end subroutine

  subroutine place_rle(field, pattern, left, top)
    logical, intent(inout) :: field(:, :)
    character(*), intent(in) :: pattern
    integer, intent(in) :: left, top
    integer :: index, run, amount, ix, iy, offset
    character(1) :: symbol

    ix = left
    iy = top
    run = 0
    do index = 1, len_trim(pattern)
      symbol = pattern(index:index)
      if (symbol >= '0' .and. symbol <= '9') then
        run = run * 10 + iachar(symbol) - iachar('0')
        cycle
      end if
      amount = max(1, run)
      run = 0
      select case (symbol)
      case ('o')
        do offset = 0, amount - 1
          if (ix + offset >= 1 .and. ix + offset <= size(field, 1) .and. &
              iy >= 1 .and. iy <= size(field, 2)) field(ix + offset, iy) = .true.
        end do
        ix = ix + amount
      case ('b')
        ix = ix + amount
      case ('$')
        iy = iy + amount
        ix = left
      case ('!')
        exit
      end select
    end do
  end subroutine

  subroutine random_field(field)
    logical, intent(out) :: field(:, :)
    call random_number(random_values)
    field = random_values < 0.25
  end subroutine

  subroutine draw_menu()
    character(96) :: lines(10)
    integer :: index, line_count

    lines = ''
    lines(1) = 'GAME OF LIFE - SCENES'
    lines(2) = 'Choose a scene with 0-5:'
    lines(3) = '0  Random field       New cells every generation'
    lines(4) = '1  Block              Still life'
    lines(5) = '2  Blinker            Oscillator, period 2'
    lines(6) = '3  Glider             Moves diagonally'
    lines(7) = '4  R-pentomino        Complex evolution from 5 cells'
    lines(8) = '5  Gosper gun         Makes a glider every 30 steps'
    lines(9) = 'M or Esc: back       Q: quit'
    lines(10) = menu_message
    line_count = min(size(lines), height + 2)
    write(output_unit, '(a)', advance='no') esc//'[2J'//esc//'[H'
    do index = 1, line_count
      if (index < line_count) then
        write(output_unit, '(a)', advance='no') trim(lines(index))//esc//'[K'//achar(13)//achar(10)
      else
        write(output_unit, '(a)', advance='no') trim(lines(index))//esc//'[K'
      end if
    end do
    flush(output_unit)
  end subroutine

  subroutine draw()
    integer :: ix, iy, live, color, previous_color, position, sequence_length
    integer(int64) :: draw_ticks
    real(8) :: draw_time, trail_age
    character(4096) :: line
    character(16) :: sequence
    character(1) :: glyph
    character(16), parameter :: palette(8) = [character(16) :: &
      achar(27)//'[38;5;15m', achar(27)//'[38;5;255m', achar(27)//'[38;5;250m', &
      achar(27)//'[38;5;245m', achar(27)//'[38;5;240m', achar(27)//'[38;5;33m', &
      achar(27)//'[38;5;27m', achar(27)//'[38;5;18m']
    call system_clock(draw_ticks)
    draw_time = real(draw_ticks - start_tick, 8) / real(tick_rate, 8)
    live = count(board)
    if (scene_id > 0) then
      write(status_line, '(a,a,i0,a,i0,a,i0,a,i0,a)') trim(scene_title), '  Gen ', generation, &
        '  Alive ', live, '  Speed ', speed, '/s  Actual ', measured_speed, '/s'
    else if (random_mode) then
      write(status_line, '(a,i0,a,i0,a,i0,a,i0,a)') 'Random  Gen ', generation, &
        '  Alive ', live, '  Speed ', speed, '/s  Actual ', measured_speed, '/s'
    else
      write(status_line, '(a,i0,a,i0,a,i0,a,i0,a)') 'Life    Gen ', generation, &
        '  Alive ', live, '  Speed ', speed, '/s  Actual ', measured_speed, '/s'
    end if
    write(output_unit, '(a)', advance='no') esc//'[H'//trim(status_line)//esc//'[K'//achar(13)//achar(10)
    if (scene_id > 0) then
      write(output_unit, '(a)', advance='no') trim(scene_description)// &
        ' M menu R reset Space +/- Q'//esc//'[K'//achar(13)//achar(10)
    else if (running) then
      write(output_unit, '(a)', advance='no') 'M scenes  Space pause  R restart  N new  C mode  T trails  +/-  Q'// &
        esc//'[K'//achar(13)//achar(10)
    else
      write(output_unit, '(a)', advance='no') 'Paused  M scenes  Space run  R restart  N new  C mode  T trails  +/-  Q'// &
        esc//'[K'//achar(13)//achar(10)
    end if
    do iy = 1, height
      position = 1
      previous_color = 0
      line(position:position) = '|'
      position = position + 1
      do ix = 1, width
        if (board(ix, iy)) then
          color = age(ix, iy)
          glyph = '#'
        else
          color = 0
          glyph = ' '
          if (show_trails) then
            trail_age = draw_time - last_death(ix, iy)
            if (trail_age < 0.2d0) then
              color = 6
            else if (trail_age < 0.45d0) then
              color = 7
            else if (trail_age < 0.9d0) then
              color = 8
            end if
            if (color /= 0) glyph = '.'
          end if
        end if
        if (color /= previous_color) then
          if (color == 0) then
            sequence = esc//'[0m'
          else
            sequence = palette(color)
          end if
          sequence_length = len_trim(sequence)
          line(position:position + sequence_length - 1) = sequence(1:sequence_length)
          position = position + sequence_length
          previous_color = color
        end if
        line(position:position) = glyph
        position = position + 1
      end do
      line(position:position + 3) = esc//'[0m'
      position = position + 4
      line(position:position) = '|'
      position = position + 1
      if (iy < height) then
        write(output_unit, '(a)', advance='no') line(:position - 1)//esc//'[K'//achar(13)//achar(10)
      else
        write(output_unit, '(a)', advance='no') line(:position - 1)//esc//'[K'
      end if
    end do
    flush(output_unit)
  end subroutine

  subroutine self_test()
    logical :: sample(5, 5), result(5, 5), expected(5, 5)
    logical :: gun(80, 40), gun_next(80, 40)
    integer :: generation_index
    sample = .false.
    sample(2:4, 3) = .true.
    expected = .false.
    expected(3, 2:4) = .true.
    call step(sample, result)
    if (.not. all(result .eqv. expected)) error stop 'Blinker test failed'
    call step(result, sample)
    if (.not. all(sample(2:4, 3))) error stop 'Blinker return test failed'
    gun = .false.
    call place_rle(gun, gosper_rle, 3, 2)
    if (count(gun) /= 36) error stop 'Gosper pattern has the wrong number of cells'
    do generation_index = 1, 45
      call step_bounded(gun, gun_next)
      gun = gun_next
    end do
    if (count(gun(29:31, 15:17)) /= 5) error stop 'Gosper gun did not make a glider'
    do generation_index = 46, 75
      call step_bounded(gun, gun_next)
      gun = gun_next
    end do
    if (count(gun(29:31, 15:17)) /= 5 .or. count(gun(37:39, 22:24)) /= 5) &
      error stop 'Gosper gun did not make a second glider'
    print '(a)', 'Life rules and Gosper glider gun: OK'
  end subroutine
end program game_of_life
