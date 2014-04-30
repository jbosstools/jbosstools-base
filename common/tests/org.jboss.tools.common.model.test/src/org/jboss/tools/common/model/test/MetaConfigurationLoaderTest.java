/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.test;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.model.ui.templates.configuration.MetaConfiguration;
import org.jboss.tools.common.model.ui.templates.configuration.MetaConfigurationManager;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.TestProjectProvider;

import junit.framework.TestCase;

public class MetaConfigurationLoaderTest extends TestCase {
	static String BUNDLE_NAME = "org.jboss.tools.common.model.test";
	TestProjectProvider provider = null;
	IProject project = null;

	public MetaConfigurationLoaderTest() {}

	public void setUp() throws Exception {
		provider = new TestProjectProvider(BUNDLE_NAME, null, "Test1", true); 
		project = provider.getProject();

		project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		
		JobUtils.waitForIdle();
	}

	public void testMetaConfigurationLoader() throws Exception {
		MetaConfiguration c = MetaConfigurationManager.getInstance().getProjectConfiguration(project);
		assertNotNull(c);
	}
	
}
