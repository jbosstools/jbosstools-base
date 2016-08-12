package org.jboss.tools.runtime.core.internal;

import org.jboss.tools.runtime.core.model.IRuntimeDetectionResolutionProvider;

public class ProblemResolutionProviderWrapper {
	private int weight;
	private String detectorFilter;
	private IRuntimeDetectionResolutionProvider provider;
	public ProblemResolutionProviderWrapper(int weight, String detectorFilter, IRuntimeDetectionResolutionProvider provider) {
		this.weight = weight;
		this.detectorFilter = detectorFilter;
		this.provider = provider;
	}
	public int getWeight() {
		return weight;
	}
	public String getDetectorFilter() {
		return detectorFilter;
	}
	public IRuntimeDetectionResolutionProvider getProvider() {
		return provider;
	}
}
