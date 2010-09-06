package org.jboss.tools.common.model.util.test;

import junit.framework.Test;

import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.test.util.ProjectImportTestSetup;

public class XProjectImportTestSetUp extends ProjectImportTestSetup {

	public XProjectImportTestSetUp(Test test, String bundleName, String projectPath, String projectName) {
		super(test,bundleName, projectPath, projectName);
	}

	public XProjectImportTestSetUp(Test test, String bundleName, String[] projectPaths, String[] projectNames) {
		super(test, bundleName, projectPaths, projectNames);
	}

	@Override
	protected void setUp() throws Exception {
		PreferenceModelUtilities.getPreferenceModel();
		super.setUp();
	}

}
