package org.jboss.tools.usage.test;

import org.jboss.tools.usage.jgoogleanalytics.ILoggingAdapter;

public class SystemOutLogger implements ILoggingAdapter {

	public void logError(String errorMessage) {
		System.out.println("errorMessage = " + errorMessage);
	}

	public void logMessage(String message) {
		System.out.println("message = " + message);
	}
}
