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

package org.jboss.tools.test.util;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.wizards.IWizardDescriptor;

/**
 * @author eskimo
 * 
 */
public class WorkbenchUtils {

	static public IWizard findWizardByDefId(String definitionId) {
		IWizardDescriptor aWizardDescr = getWorkbench().getNewWizardRegistry()
				.findWizard(definitionId);
		TestCase.assertNotNull("Cannot find wizard " + definitionId //$NON-NLS-1$
				+ " definition in wizard registry", aWizardDescr); //$NON-NLS-1$
		IWorkbenchWizard aWizard = null;
		try {
			aWizard = aWizardDescr.createWizard();
		} catch (CoreException e) {
			JUnitUtils.fail("Cannot create IWorkbenchWizard instance", e); //$NON-NLS-1$
		}
		return aWizard;
	}

	public static IWorkbench getWorkbench() {
		return PlatformUI.getWorkbench();
	}

	/**
	 * @return
	 */
	public static IWorkbenchPage getWorkbenchActivePage() {
		return getWorkbench().getActiveWorkbenchWindow().getActivePage();
	}

	/**
	 * To show modal dialog parent shell is required, this method can be used to
	 * obtain active workbench window shell
	 * 
	 * @return active workbench window shell
	 */
	public static Shell getActiveShell() {
		return getWorkbench().getActiveWorkbenchWindow().getShell();
	}

	public static PreferenceManager getPreferenceManager() {
		return getWorkbench().getPreferenceManager();
	}

	public static PreferenceDialog createPreferenceDialog(String pageId) {
		PreferenceDialog dialog = PreferencesUtil.createPreferenceDialogOn(WorkbenchUtils
				.getActiveShell(), pageId, new String[] {pageId}, null);
		dialog.setBlockOnOpen(false);
		return dialog;
	}

	public static PreferenceDialog createPropertyDialog(String pageId,
			IProject project) {
		return PreferencesUtil.createPropertyDialogOn(WorkbenchUtils
				.getActiveShell(), project, pageId, new String[] {pageId}, null);
	}
	
	public static IEditorPart openEditor(String inputFile) {
		IEditorPart part = null;
		try {
			part = IDE.openEditor(getWorkbenchActivePage(),ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(inputFile)));
		} catch (PartInitException e) {
			e.printStackTrace();
		}
		return part;
	}
	
	public static IEditorPart openEditor(String inputFile, String editorId) {
		IEditorPart part = null;
		try {
			part = IDE.openEditor(getWorkbenchActivePage(),ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(inputFile)),editorId);
		} catch (PartInitException e) {
			e.printStackTrace();
		}
		return part;
	}
	
	public static IEditorPart openEditor(IFile inputFile, String editorId) {
		IEditorPart part = null;
		try {
			part = IDE.openEditor(getWorkbenchActivePage(),inputFile,editorId);
		} catch (PartInitException e) {
			e.printStackTrace();
		}
		return part;
	}

	public static void closeAllEditors() {
		getWorkbenchActivePage().closeAllEditors(false);
	}
}