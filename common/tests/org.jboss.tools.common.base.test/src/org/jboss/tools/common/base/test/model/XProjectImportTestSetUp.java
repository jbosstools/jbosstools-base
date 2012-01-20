/*******************************************************************************
  * Copyright (c) 2010 - 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/

package org.jboss.tools.common.base.test.model;

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
