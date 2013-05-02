package org.jboss.tools.runtime.core.internal;

import java.util.ResourceBundle;

/**
 * This class will no longer be used once the 
 * downloadRuntimeProvider extension point is the 
 * agreed-upon workflow. 
 */
public class ExternalRuntimeDownload {

	private static final String JBOSS_RUNTIME_URL_DEFAULT = "http://download.jboss.org/jbosstools/examples/download_runtimes.xml"; //$NON-NLS-1$
	
	private static final String JBOSS_RUNTIME_DIRECTORY = "jboss.runtime.directory.url"; //$NON-NLS-1$
	
	private static final String JBOSS_RUNTIME_URL;

	static {
		ResourceBundle rb = ResourceBundle.getBundle("org.jboss.tools.runtime.core.internal.runtime"); //$NON-NLS-1$
		String url = rb.getString("runtime.url").trim(); //$NON-NLS-1$
		if ("".equals(url) || "${jboss.runtime.directory.url}".equals(url)){  //$NON-NLS-1$//$NON-NLS-2$
			//was not filtered, fallback to default value
			JBOSS_RUNTIME_URL = JBOSS_RUNTIME_URL_DEFAULT;
		} else {
			JBOSS_RUNTIME_URL = url;
		}
	}
	
	public static String getURL() {
		String directory = System.getProperty(JBOSS_RUNTIME_DIRECTORY, null);
		if (directory == null) {
			// else use Maven-generated value (or fall back to default)
			return JBOSS_RUNTIME_URL;
		}
		return directory;		
	}
}
