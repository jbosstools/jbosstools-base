package org.jboss.tools.usage.googleanalytics;

import org.jboss.tools.usage.IHumanReadable;

public interface IJBossToolsEclipseEnvironment extends IGoogleAnalyticsParameters, IHumanReadable {

	public String getJBossToolsVersion();
}
