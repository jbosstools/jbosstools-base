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

import java.lang.reflect.Modifier;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.wizards.NewElementWizard;
import org.eclipse.jdt.ui.wizards.NewTypeWizardPage;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.ui.wizard.service.NewServiceCreationWizard;
import org.jboss.tools.common.ui.wizard.service.NewServiceWizardPage;
import org.jboss.tools.common.util.EclipseJavaUtil;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.test.util.JUnitUtils;
import org.jboss.tools.test.util.ResourcesUtils;
import org.jboss.tools.test.util.WorkbenchUtils;

/**
 * @author Viacheslav Kabanovich
 *
 */
public class NewServiceProviderWizardTest extends TestCase {
	
	static String PACK_NAME = "test";
	static String EXISTING_PACK_NAME = "org.jboss.jsr299.tck.tests.jbt.validation.target";
	static String EXISTING_INTERCEPTOR_BINDING_NAME = "InterceptorBindingWTypeTarget";  // @Inherited @Target({TYPE})
	static String SERVICE_NAME = "MyService";
	
	static class WizardContext {
		NewElementWizard wizard;
		IProject tck;
		IJavaProject jp;
		WizardDialog dialog;
		NewTypeWizardPage page;
		String packName;
		String typeName;
		

		public void init(String wizardId, String packName, String typeName) {
			this.packName = packName;
			this.typeName = typeName;
			wizard = (NewElementWizard)WorkbenchUtils.findWizardByDefId(wizardId);
			tck = ResourcesPlugin.getWorkspace().getRoot().getProject("Test");
			jp = EclipseUtil.getJavaProject(tck);
			wizard.init(CommonUIPlugin.getDefault().getWorkbench(), new StructuredSelection(jp));
			if(wizard instanceof NewServiceCreationWizard) {
			    ((NewServiceCreationWizard)wizard).setOpenEditorAfterFinish(false);
			}
			dialog = new WizardDialog(
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
					wizard);
			dialog.setBlockOnOpen(false);
			dialog.open();

			page = (NewTypeWizardPage)dialog.getSelectedPage();

			page.setTypeName(typeName, true);
			IPackageFragment pack = page.getPackageFragmentRoot().getPackageFragment(PACK_NAME);
			page.setPackageFragment(pack, true);
		}

		public String getNewTypeContent() {
			IType type = null;
			try {
				String tn = typeName;
				int q = tn.indexOf("<");
				if(q >= 0) tn = tn.substring(0, q);
				type = jp.findType(packName + "." + tn);
			} catch (JavaModelException e) {
				JUnitUtils.fail("Cannot find type " + typeName, e);
			}
			
			IFile file = (IFile)type.getResource();
			assertNotNull(file);
			String text = null;
			try {
				text = FileUtil.readStream(file.getContents());
			} catch (CoreException e) {
				JUnitUtils.fail("Cannot read from " + file, e);
			}
			return text;
		}

		public void close() {
			dialog.close();
		}
		
	}

	public void testNewServiceWizard() throws Exception {
		WizardContext context = new WizardContext();
		context.init("org.jboss.tools.common.ui.wizard.service.NewServiceCreationWizard",
				PACK_NAME, SERVICE_NAME);

		try {
			NewServiceWizardPage page = (NewServiceWizardPage)context.page;
			
			String serviceType = "java.util.List111";
			page.setServiceType(serviceType);			
			String message = page.getErrorMessage();			
			String expectedMessage = NLS.bind(CommonUIMessages.NEW_SERVICE_WIZARD_SERVICE_TYPE_NOT_EXISTS, serviceType);
			assertEquals(expectedMessage, message);
			
			serviceType = "java.util.List";
			page.setServiceType(serviceType);
			assertNull(page.getErrorMessage());
			
			context.wizard.performFinish();
			
			String text = context.getNewTypeContent();
			
			assertTrue(text.contains("@Override"));
			assertTrue(text.contains("iterator()"));
			
			IType type = (IType)context.wizard.getCreatedElement();
			int f = type.getFlags();
			assertTrue(Modifier.isPublic(f));
			assertFalse(Modifier.isAbstract(f));
			String[] is = type.getSuperInterfaceNames();
			assertEquals(1, is.length);
			assertEquals("List", is[0]);

			IResource[] srcs = EclipseUtil.getJavaSourceRoots(type.getJavaProject().getProject());
			IFile file = ((IFolder)srcs[0]).getFile(new Path("META-INF/services/java.util.List"));
			assertTrue(file.exists());
			String content = FileUtil.readStream(file);
			assertEquals(type.getFullyQualifiedName(), content.trim());
		} finally {
			context.close();
		}
	}

	public void setUp() {
		try {
			IProject p = ResourcesUtils.importProject("org.jboss.tools.common.ui.test", "projects/Test");
			System.out.println(p);
		} catch (Exception e) {
			fail();
		}
	}
}