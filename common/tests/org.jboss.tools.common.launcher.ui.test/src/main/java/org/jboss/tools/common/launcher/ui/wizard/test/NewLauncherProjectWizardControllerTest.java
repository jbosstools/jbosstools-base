/*************************************************************************************
 * Copyright (c) 2018 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.launcher.ui.wizard.test;

import static org.junit.Assert.assertTrue;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.launcher.core.model.Catalog;
import org.jboss.tools.common.launcher.core.model.CatalogManager;
import org.jboss.tools.common.launcher.ui.wizard.NewLauncherProjectModel;
import org.jboss.tools.common.launcher.ui.wizard.NewLauncherProjectWizardController;
import org.junit.BeforeClass;
import org.junit.Test;

public class NewLauncherProjectWizardControllerTest {
	private int counter = 0;
	
	private static Catalog catalog;
	
	@BeforeClass
	public static void setup() throws CoreException {
		catalog = CatalogManager.getDefault().getCatalog(new NullProgressMonitor());
	}
	
	@Test
	public void testDefaultBooster()  {
		NewLauncherProjectModel model = new NewLauncherProjectModel();
		model.setProjectName("launcher" + counter++);
		model.setCatalog(catalog);
		IStatus status = new NewLauncherProjectWizardController(model).run(new NullProgressMonitor());
		assertTrue(status.isOK());
	}
}
