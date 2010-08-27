/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;

import org.jboss.tools.usage.GlobalUsageReportingSettings;
import org.junit.Test;

/**
 * Test for the global usage report settings. All tests are disabled yet,
 * implementation's unfortunately still buggy.
 */
public class GlobalUsageReportingSettingsTest {

	@Test
	public void canExtractEnabledValue() throws IOException {
		GlobalReportingSettingsFake reportSettings = new GlobalReportingSettingsFake("ENABLED", "", "");
		assertTrue(reportSettings.isEnabled());
	}

	@Test
	public void canExtractDisabledValue() throws IOException {
		GlobalReportingSettingsFake reportSettings = new GlobalReportingSettingsFake("DISABLED", "", "");
		assertFalse(reportSettings.isEnabled());
	}

	@Test
	public void canExtractDisabledOutUndefinedValue() throws IOException {
		GlobalReportingSettingsFake reportEnablement = new GlobalReportingSettingsFake("Rubbish", "", "");
		assertFalse(reportEnablement.isEnabled());
	}

	@Test
	public void canExtractStringValue() throws IOException {
		GlobalReportingSettingsFake reportSettings = new GlobalReportingSettingsFake("", "dummy", "");
		assertEquals("dummy", reportSettings.getStringValue());
	}

	@Test
	public void canExtractIntegerValue() throws IOException {
		GlobalReportingSettingsFake reportSettings = new GlobalReportingSettingsFake("", "", "42");
		assertEquals(Integer.valueOf(42), reportSettings.getIntegerValue());
	}

	private class GlobalReportingSettingsFake extends GlobalUsageReportingSettings {

		private String enablementValue;
		private String integerValue;
		private String stringValue;

		public GlobalReportingSettingsFake(String enablementValue, String dummyValue, String anotherValue)
				throws IOException {
			super(JBossToolsUsageTestActivator.getDefault());
			this.enablementValue = enablementValue;
			this.stringValue = dummyValue;
			this.integerValue = anotherValue;
		}

		@Override
		protected InputStreamReader request(String url) throws UnsupportedEncodingException {
			return new InputStreamReader(new ByteArrayInputStream(getEnablementPageContent(enablementValue,
					stringValue, integerValue).getBytes()), "UTF-8");
		}
	}

	private String getEnablementPageContent(String enablementValue, String dummyValue, String integerValue) {

		return 
//				"Dummy Value would be cool but here follows the Boolean Value for the Report Enablement: "
//				+ "<h1>" + GlobalUsageReportingSettings.KEY_REPORT_ENABLEMENT
//				+ enablementValue
//				+ "</h1>"
//
//				+ "The Dummy Value in this Resource is set to "
//				+ "<h1>" + GlobalUsageReportingSettings.KEY_DUMMY_VALUE
//				+ dummyValue
//				+ "</h1>"
//
//				+ "Boolean Usage Reporting is set to:"
//				+ "<h1>" + GlobalUsageReportingSettings.KEY_REPORT_ENABLEMENT
//				+ enablementValue
//				+ "</h1>"
//
//				+ "And the value of type Integer is "
//				+ "<h1>" + GlobalUsageReportingSettings.KEY_INTEGER_VALUE
//				+ integerValue
//				+ "</h1>"
//				+ " is the Value that is a String";
		 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\""
		 + "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"> "
		 + " "
		 + " "
		 +
		 "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\"> "
		 + "<head> "
		 +
		 "    <title> JBoss Tools / JBoss Developer Studio Usage Reporting Enablement - JBoss Community</title> "
		 + " "
		 +
		 "    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=EmulateIE7\" /> "
		 + " "
		 + "    <script type=\"text/javascript\"> "
		 + "    	var javascriptIsCool = false;"
		 + "    </script> "
		 + "    "
		 + "</head> "
		 + "<body class=\"jive-body-content jive-body-content-document\" > "
		 + "        <div id=\"jive-body\"> "
		 + " "
		 + "<div class=\"jive-content\"> "
		 + "    <div class=\"jive-content-header\"> "
		 +
		 "        <div class=\"jive-wiki-post-moderating jive-content-header-moderating\"> "
		 +
		 "            <span class=\"jive-icon-med jive-icon-moderation\"></span>Currently Being Moderated"
		 + "        </div> "
		 + "        <div class=\"jive-content-title\"> "
		 +
		 "            <h2><span class=\"jive-icon-big jive-icon-document\"></span> JBoss Tools / JBoss Developer Studio Usage Reporting Enablement</h2> "
		 + "        </div> "
		 + "        <div class=\"jive-content-header-version\"> "
		 + "            VERSION 5&nbsp;"
		 +
		 "                <a href=\"/wiki/JBossToolsJBossDeveloperStudioUsageReportingEnablement/diff?secondVersionNumber=5\" title=\"Click to view article history\"><img class=\"jive-icon-sml jive-icon-search\" src=\"/4.0.5/images/transparent.png\" alt=\"Click to view article history\" /></a> "
		 + "        </div> "
		 + "		<div class=\"jive-content-header-details\"> "
		 + " "
		 + "Created on: Aug 24, 2010 5:39 AM by"
		 + "<a href=\"/people/adietish\""
		 + "id=\"jive-72036899,987,346,482,238\""
		 + "onmouseover=\"quickuserprofile.getUserProfileTooltip(72036);\""
		 + "onmouseout=\"quickuserprofile.cancelTooltip();\""
		 + "class=\"jiveTT-hover-user jive-username-link\""
		 + ">Andre Dietisheim</a>            <span>-</span> "
		 + "Last Modified:&nbsp;"
		 + "Aug 24, 2010 5:53 AM"
		 + "by <a href=\"/people/adietish\""
		 + "id=\"jive-72036899,987,347,353,238\""
		 + "onmouseover=\"quickuserprofile.getUserProfileTooltip(72036);\""
		 + "onmouseout=\"quickuserprofile.cancelTooltip();\""
		 + "class=\"jiveTT-hover-user jive-username-link\""
		 + ">Andre Dietisheim</a>		</div> "
		 + " "
		 + "	</div> "
		 + "	<div class=\"jive-content-body\"> "
		 + " "
		 +
		 "<!-- [DocumentBodyStart:e26c60c0-cb73-47b7-bded-f4eb7320305b] --><div class=\"jive-rendered-content\"><p>This article is queried by the JBoss Tools / JBoss Developer Studio usage reporting plugin. It implements a global kill-switch that allows us to disable usage reporting stats. The plugin looks for a string of the format:</p><p style=\"min-height: 8pt; height: 8pt; padding: 0px;\">&#160;</p><p><strong>Usage&#160; Reporting&#160; is &lt;"
		 +
		 "ENABLED&gt;</strong>. Any value that differs from ENABLED is interpreted as DISABLED.</p><p style=\"min-height: 8pt; height: 8pt; padding: 0px;\">&#160;</p><h1>Usage Reporting is "
		
		 + enablementValue
		
		 + "</h1>"
		 + "<h1>Dummy Value is "
		
		 + dummyValue
		
		 + "</h1>"
		
		 + "<h1>Integer Value is "
		
		 + integerValue
		
		 + "</h1>"
		
		 +
		 "</div><!-- [DocumentBodyEnd:e26c60c0-cb73-47b7-bded-f4eb7320305b] --> "
		 + " "
		 + "	</div> "
		 + "    <div class=\"jive-content-footer\"> "
		 + " "
		 + " "
		 + "    <!-- BEGIN content details --> "
		 + "        <span class=\"jive-content-footer-item\"> "
		 + "            18&nbsp;Views</a> "
		 + "        </span> "
		 + "  "
		 + " "
		 + "    </div> "
		 + "</div> "
		 + "</body> "
		 + "</html> ";
	}

	@Test
	public void isPageAccessible() throws IOException {
		GlobalUsageReportingSettings reportEnablement = new GlobalUsageReportingSettings(JBossToolsUsageTestActivator
				.getDefault());
		System.err.println("Usage reporting is globally \"" + reportEnablement.isEnabled() + "\"");
	}
}
