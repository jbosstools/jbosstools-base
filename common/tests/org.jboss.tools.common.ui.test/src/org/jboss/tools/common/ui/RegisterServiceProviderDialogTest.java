/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.ui;

import junit.framework.TestCase;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.ui.wizard.service.RegisterAsServiceDialog;
import org.jboss.tools.common.ui.wizard.service.RegisterServiceUtil;
import org.jboss.tools.common.util.EclipseJavaUtil;
import org.jboss.tools.test.util.ResourcesUtils;

/**
 * @author Viacheslav Kabanovich
 *
 */
public class RegisterServiceProviderDialogTest extends TestCase {
	static String SERVICE_NAME = "test.ListProvider";
	IProject project;
	
	static class WizardContext {
		IJavaProject jp;
		RegisterAsServiceDialog dialog;
		String typeName;
		IType type;
		
		public void init(String typeName) throws CoreException {
			this.typeName = typeName;
			IProject tck = ResourcesPlugin.getWorkspace().getRoot().getProject("Test");
			jp = EclipseUtil.getJavaProject(tck);
			type = EclipseJavaUtil.findType(jp, typeName);
			dialog = new RegisterAsServiceDialog(
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), type);
			dialog.setBlockOnOpen(false);
			dialog.open();
		}

		public void close() {
			dialog.close();
		}
	}

	public void testRegisterAsService() throws Exception {
		WizardContext context = new WizardContext();
		context.init(SERVICE_NAME);

		try {
			RegisterAsServiceDialog dialog = context.dialog;
			
			dialog.setServiceType("java.util.ArrayList");
			assertEquals(CommonUIMessages.REGISTER_AS_SERVICE_NON_ABSTRACT_MESSAGE, dialog.getMessage());

			dialog.setServiceType("java.util.List");
			assertEquals(CommonUIMessages.REGISTER_AS_SERVICE_MESSAGE, dialog.getMessage());
			dialog.okPressed();
			String result = dialog.getResult();
		    try {
	    		RegisterServiceUtil.registerService(context.type, result);
		    } catch (CoreException e) {
		    	CommonUIPlugin.getDefault().logError(e);
		    }
		    assertEquals("java.util.List", result);
			
		    assertTrue(RegisterServiceUtil.isServiceRegistered(project, SERVICE_NAME, result));

		} finally {
			context.close();
		}

		//2. Repeat and check same service type.
		context = new WizardContext();
		context.init(SERVICE_NAME);
		try {
			RegisterAsServiceDialog dialog = context.dialog;
			dialog.setServiceType("java.util.List");
			assertEquals(CommonUIMessages.REGISTER_AS_SERVICE_ALREADY_REGISTERED_MESSAGE, dialog.getErrorMessage());

			dialog.setServiceType("java.util.AbstractList");
			assertNull(dialog.getErrorMessage());
			assertEquals(CommonUIMessages.REGISTER_AS_SERVICE_MESSAGE, dialog.getMessage());
		} finally {
			context.close();
		}
	}

	public void setUp() {
		try {
			project = ResourcesUtils.importProject("org.jboss.tools.common.ui.test", "projects/Test");
		} catch (Exception e) {
			fail();
		}
	}

	public void tearDown() {
		try {
			project.delete(true, new NullProgressMonitor());
		} catch (Exception e) {
			fail();
		}
	}
}
