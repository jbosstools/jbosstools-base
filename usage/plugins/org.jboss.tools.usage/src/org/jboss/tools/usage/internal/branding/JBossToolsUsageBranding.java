package org.jboss.tools.usage.internal.branding;

import org.jboss.tools.usage.branding.IUsageBranding;

public class JBossToolsUsageBranding implements IUsageBranding {

	public String getPreferencesDescription() {
		return JBossToolsUsageBrandingMessages.UsageReportPreferencePage_Description;
	}

	public String getPreferencesAllowReportingCheckboxLabel() {
		return JBossToolsUsageBrandingMessages.UsageReportPreferencePage_AllowReporting;
	}
	
	public String getStartupAllowReportingTitle() {
		return JBossToolsUsageBrandingMessages.UsageReport_DialogTitle;
	}
	
	public String getStartupAllowReportingMessage() {
		return JBossToolsUsageBrandingMessages.UsageReport_DialogMessage;
	}

	public String getStartupAllowReportingCheckboxLabel() {
		return JBossToolsUsageBrandingMessages.UsageReport_Checkbox_Text;
	}
	
	public String getStartupAllowReportingDetailLink() {
		return JBossToolsUsageBrandingMessages.UsageReport_ExplanationPage;
	}
	
	public String getGlobalRemotePropertiesUrl() {
		return JBossToolsUsageBrandingMessages.GlobalUsageSettings_RemoteProps_URL;
	}

	public String getGoogleAnalyticsAccount() {
		return JBossToolsUsageBrandingMessages.UsageReport_GoogleAnalytics_Account;
	}

	public String getGoogleAnalyticsReportingHost() {
		return JBossToolsUsageBrandingMessages.UsageReport_HostName;
	}
}
