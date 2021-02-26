export type Startup = null | StartupItem;
export type StartupItem = [ string, StartupItem1 ];
export type StartupItem1 = null | string;
