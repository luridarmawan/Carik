program carik_test;
{
  Carik unit tests.

  Jalankan tanpa compile manual:
    Windows : run_tests.bat
    Linux   : ./run_tests.sh

  Butuh FastPlaz (env FASTPLAZ_DIR, default ../../../vendors/fastplaz/FastPlaz,
  sama dengan extra.cfg). Runner (fastplaz_testrunner) dipakai dari folder test FastPlaz.
}

{$mode objfpc}{$H+}

uses
  {$IFNDEF Windows}cthreads,{$ENDIF}
  Classes, consoletestrunner, fastplaz_testrunner, test_security_lib;

type

  { TMyTestRunner }

  TMyTestRunner = class(TFastPlazTestRunner)
  protected
  end;

var
  App: TMyTestRunner;

begin
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;
  App := TMyTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'Carik - Unit Test';
  App.Run;
  App.Free;
end.
