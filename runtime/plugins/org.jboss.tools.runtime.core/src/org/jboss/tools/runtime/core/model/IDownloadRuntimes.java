package org.jboss.tools.runtime.core.model;

import java.util.HashMap;

public interface IDownloadRuntimes {
	/**
	 * Should be a ui Shell object 
	 */
	public static final String SHELL = "download.runtimes.shell"; //$NON-NLS-1$
	
	/**
	 * May be used to limit the number of items showing up in the
	 * download runtime dialog. 
	 */
	public static final String RUNTIME_FILTER = "download.runtimes.filter"; //$NON-NLS-1$

	/**
	 * Is the download initialized (true) or canceled (false)?
	 */
	public static final String DOWNLOAD_LAUNCHED = "is.download.launched"; //$NON-NLS-1$
	
	
	public void execute(HashMap<String, Object> data);
}
