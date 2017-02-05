program line;

{$mode objfpc}{$H+}

uses
  cthreads,
  fpcgi, sysutils, fastplaz_handler, common, main, routes, carik_webmodule;

begin
  Application.Title:='Carik - LINE Bot';
  Application.Email := string( Config.GetValue(_SYSTEM_WEBMASTER_EMAIL,'webmaster@' + GetEnvironmentVariable('SERVER_NAME')));
  Application.DefaultModuleName := string( Config.GetValue(_SYSTEM_MODULE_DEFAULT, 'main'));
  Application.ModuleVariable := string( Config.GetValue(_SYSTEM_MODULE_VARIABLE, 'mod'));
  Application.AllowDefaultModule := True;
  Application.RedirectOnErrorURL := string( Config.GetValue(_SYSTEM_ERROR_URL, '/'));
  Application.RedirectOnError:= Config.GetValue( _SYSTEM_ERROR_REDIRECT, false);

  Application.OnGetModule := @FastPlasAppandler.OnGetModule;
  Application.PreferModuleName := True;

  Application.Initialize;
  Application.Run;
end.
