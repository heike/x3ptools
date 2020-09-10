# installs dependencies, runs R CMD check, runs covr::codecov()

get_stage("before_install") %>%
  add_code_step(options(rgl.useNULL = TRUE)) %>%
  add_code_step(system("/sbin/start-stop-daemon --start --quiet --pidfile /tmp/custom_xvfb_99.pid --make-pidfile --background --exec /usr/bin/Xvfb -- :99 -ac -screen 0 1280x1024x24"))

get_stage("before_script") %>%
  add_code_step(system("export DISPLAY=:99.0")) %>%
  add_code_step(system("sh -e /etc/init.d/xvfb start")) %>%
  add_code_step(system("sleep 3"))

do_package_checks()
