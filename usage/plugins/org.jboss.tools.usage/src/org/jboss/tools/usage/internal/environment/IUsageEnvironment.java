/*******************************************************************************
 * Copyright (c) 2010-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal.environment;

/**
 * An interface that provides methods for all parameters that google analytics
 * needs to know about.
 * 
 * @author Andre Dietisheim
 * @see <a
 *      href="http://code.google.com/apis/analytics/docs/tracking/gaTrackingTroubleshooting.html#gifParameters">GIF
 *      Request Parameters</a>
 */
public interface IUsageEnvironment {

	public String getScreenResolution();

	public String getScreenColorDepth();

	public String getBrowserLanguage();

	public String getUserAgent();

	public String getUserId();

	public String getKeyword();

	public String getFirstVisit();

	public String getLastVisit();

	public String getCurrentVisit();

	public long getVisitCount();

	/**
	 * Signals that a visit was executed. The
	 * consequence is that visit timestamps and visit counters get updated
	 * 
	 * @see #getLastVisit()
	 * @see #getCurrentVisit()
	 * @see #getVisitCount()
	 */
	public void visit();

	/**
	 * Starts a new visit session. 
	 */
	public void startNewVisitSession();

	public String getFlashVersion();

	/**
	 * Returns a user defined value that may be queried in Google Analytics.
	 *
	 * @return a user defined value
	 */
	public String getUserDefined();

	/**
	 * TODO: support multiple events. 
	 */
	public UsageEventValue getEvent();
	
	public String getCentralEnabledValue();

	public String getJavaVmName();

	public String getJavaVendor();

	public String getJavaBitVersion();
	
	public static class UsageEventValue {
		
		private String name;
		private String label;
		private String value;

		public UsageEventValue(String name, String label, String value) {
			this.name = name;
			this.label = label;
			this.value = value;
		}

		public String getName() {
			return name;
		}

		public String getLabel() {
			return label;
		}

		public String getValue() {
			return value;
		}
	}

}