package org.jboss.tools.usage.test;

import org.jboss.tools.usage.googleanalytics.ILoggingAdapter;

/**
 * @author Andre Dietisheim
 */
public class SystemOutLogger implements ILoggingAdapter {

	public void logError(String errorMessage) {
		System.out.println("[DEBUG]: " + errorMessage);
	}

	public void logMessage(String message) {
		System.out.println("[ERROR]" + message);
	}
}
