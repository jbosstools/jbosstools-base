package org.jboss.tools.runtime.core.model;

public interface IDownloadRuntimeFilter {
	public boolean accepts(DownloadRuntime runtime);
	public DownloadRuntime[] filter(DownloadRuntime[] runtime);
}
