--This should be run with the command M2 --script m2r_server_script.m2 PORT#

if #commandLine < 4 then
  error("CommandLine was not given port.");

m2rintinout = openInOut concatenate("$:", commandLine#3);
m2rintruncount = 0;
m2rintinout << "1.0.0" << "\n" << flush;
while true do (
  m2rintinline = read m2rintinout;
  if m2rintinline == "" then break;

  m2rintretcode = 0;
  m2rintoutvalsucceeded = false;
  m2rintoutlinesucceeded = false;
  m2rintruncount = m2rintruncount + 1;

  try (
    m2rintoutval_m2rintruncount = value(m2rintinline);
    m2rintoutvalsucceeded = true;

    m2rintoutclass = class m2rintoutval_m2rintruncount;
    m2rintoutclassclass = class m2rintoutclass;

    m2rintvarname = "m2o" | toString(m2rintruncount);
    value(m2rintvarname | " = m2rintoutval_m2rintruncount;");

    m2rintoutline = toExternalString m2rintoutval_m2rintruncount;
    m2rintoutlinesucceeded = true;
  );

  if not m2rintoutvalsucceeded then (
    m2rintoutline = "Macaulay2 Error!";
    m2rintretcode = 1;
  ) else if not m2rintoutlinesucceeded then (
    m2rintoutline = "Macaulay2 toExternalString Error!";
    m2rintretcode = 2;
  );

  m2rintnumlines = 1 + #select("\n", m2rintoutline);

  m2rintinout << m2rintretcode << " " << m2rintnumlines << " "
              << toString(m2rintvarname) << " "
              << toString(m2rintoutclass) << " "
              << toString(m2rintoutclassclass) << "\n"
              << m2rintoutline << "\n" << flush;
);
close m2rintinout;
