/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.jboss.tools.common.ui.propertytester.internal.NaturePropertyTester;
import org.jboss.tools.test.util.ProjectImportTestSetup;

@SuppressWarnings("restriction")
public class NaturePropertyTesterTests extends TestCase{
	private static final String PROJECT_FOLDER_NAME = "projects/"; //$NON-NLS-1$
	private static final String SIMPLE_PROJECT_NAME = "TestProject"; //$NON-NLS-1$
	private static final String FILE1_NAME =  "/folder/test.txt"; //$NON-NLS-1$
	private static final String JAVA_PROJECT_NAME = "Test"; //$NON-NLS-1$
	private static final String FILE2_NAME =  "/src/test/ListProvider.java"; //$NON-NLS-1$
	
	public IProject[] projects = new IProject[2];

	protected void setUp() throws Exception {
		if(projects[0] == null || projects[1] == null){
			ProjectImportTestSetup setup = new ProjectImportTestSetup(null,
					"org.jboss.tools.common.ui.test", //$NON-NLS-1$
					new String[]{PROJECT_FOLDER_NAME+SIMPLE_PROJECT_NAME, PROJECT_FOLDER_NAME+JAVA_PROJECT_NAME},
					new String[]{SIMPLE_PROJECT_NAME, JAVA_PROJECT_NAME});
			projects = setup.importProjects();
		}
		
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeAllEditors(false);
	}
	
	protected void tearDown() {
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeAllEditors(false);
	}
	
	public NaturePropertyTesterTests() {
		super("Nature Property Tester Test"); //$NON-NLS-1$
	}
	
	public void testSimpleProject() throws PartInitException{
		checkPropertyTester(projects[0], FILE1_NAME, false);
	}

	public void testJavaProject() throws PartInitException{
		checkPropertyTester(projects[1], FILE2_NAME, true);
	}

	
	private static void checkPropertyTester(IProject project, String fileName, boolean javaExpected) throws PartInitException{
		assertTrue("project does not exist", project.exists());  //$NON-NLS-1$
		IFile file = project.getFile(fileName);
		
		assertTrue("File - "+file.getFullPath()+" not found", file.exists());  //$NON-NLS-1$ //$NON-NLS-2$
		
		IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IEditorPart editor = IDE.openEditor(page, file);
		
		NaturePropertyTester tester = new NaturePropertyTester();
		
		boolean result = tester.test(editor, NaturePropertyTester.PROPERTY_NAME, null, "org.eclipse.jdt.core.javanature"); //$NON-NLS-1$
		
		assertEquals("Wrong nature detection for project -"+project.getName(), javaExpected, result); //$NON-NLS-1$
	}
}
