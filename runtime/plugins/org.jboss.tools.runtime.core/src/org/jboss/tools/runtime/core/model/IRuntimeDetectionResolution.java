package org.jboss.tools.runtime.core.model;

public interface IRuntimeDetectionResolution {
    /**
     * Returns a short label indicating what the resolution will do.
     *
     * @return a short label for this resolution
     */
    public String getLabel();

    /**
     * Runs this resolution.
     *
     * @param problem the problem to resolve
     * @param definition the runtime definition affected by this problem
     */
    public void run(RuntimeDetectionProblem problem, RuntimeDefinition definition);

}
