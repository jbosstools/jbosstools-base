package org.jboss.tools.runtime.core.model;

public interface IRuntimeDetectionResolutionProvider {
	
	/**
	 * Return a list of resolutions, or if none apply to the given problem, an empty array.
	 * 
	 * @param problem 	The problem to be fixed
	 * @param def 		The runtime definition experiencing the error
	 * @return
	 */
	public IRuntimeDetectionResolution[] getResolutions(RuntimeDetectionProblem problem, RuntimeDefinition def);
}
