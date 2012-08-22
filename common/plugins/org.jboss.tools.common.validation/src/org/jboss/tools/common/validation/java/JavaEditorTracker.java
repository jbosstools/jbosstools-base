/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.validation.java;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor;
import org.eclipse.jdt.internal.ui.javaeditor.JavaSourceViewer;
import org.eclipse.jdt.ui.text.IJavaPartitions;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPageListener;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.validation.CommonValidationPlugin;

/**
 * Installer for As-You-Type validation
 * 
 * @author Victor V. Rubezhny
 *
 */
@SuppressWarnings("restriction")
public class JavaEditorTracker implements IWindowListener, IPageListener, IPartListener {
	static JavaEditorTracker INSTANCE;

	Map<JavaEditor, JavaDirtyRegionProcessor> fAsYouTypeValidators = new HashMap<JavaEditor, JavaDirtyRegionProcessor>(); 

	private JavaEditorTracker() {
		init();
	}

	public static JavaEditorTracker getInstance() {
		if(INSTANCE == null) {
			INSTANCE = new JavaEditorTracker();
		}
		return INSTANCE;
	}

	private void init() {
		if(PlatformUI.isWorkbenchRunning()) {
			IWorkbench workbench = CommonValidationPlugin.getDefault().getWorkbench();
			if(workbench != null) {
				IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();
				for (IWorkbenchWindow window: windows) {
					windowOpened(window);
				}
				CommonValidationPlugin.getDefault().getWorkbench().addWindowListener(this);
			}
		}
	}

	@Override
	public void windowActivated(IWorkbenchWindow window) {
	}

	@Override
	public void windowDeactivated(IWorkbenchWindow window) {
	}

	@Override
	public void windowClosed(IWorkbenchWindow window) {
		IWorkbenchPage[] pages = window.getPages();
		for (IWorkbenchPage page: pages) {
			pageClosed(page);
		}
		window.removePageListener(this);
	}

	@Override
	public void windowOpened(IWorkbenchWindow window) {
		if(window.getShell() != null) {
			IWorkbenchPage[] pages = window.getPages();
			for (IWorkbenchPage page: pages) {
				pageOpened(page);
			}
			window.addPageListener(this);
		}
	}

	@Override
	public void pageActivated(IWorkbenchPage page) {
	}

	@Override
	public void pageClosed(IWorkbenchPage page) {
		IEditorReference[] rs = page.getEditorReferences();
		for (IEditorReference r: rs) {
			IEditorPart part = r.getEditor(false);
			if(part != null) {
				editorClosed(part);
			}
		}
		page.removePartListener(this);
	}

	@Override
	public void pageOpened(IWorkbenchPage page) {
		IEditorReference[] rs = page.getEditorReferences();
		for (IEditorReference r: rs) {
			IEditorPart part = r.getEditor(false);
			if(part != null) {
				editorOpened(part);
			}							
		}
		page.addPartListener(this);
	}

	@Override
	public void partActivated(IWorkbenchPart part) {
	}

	@Override
	public void partBroughtToTop(IWorkbenchPart part) {
	}

	@Override
	public void partClosed(IWorkbenchPart part) {
		if (part instanceof IEditorPart) {
			editorClosed((IEditorPart)part);
		}
	}

	@Override
	public void partDeactivated(IWorkbenchPart part) {
	}

	@Override
	public void partOpened(IWorkbenchPart part) {
		if (part instanceof IEditorPart) {
			editorOpened((IEditorPart)part);
		}
	}

	private void editorOpened(IEditorPart part) {
		if (part instanceof JavaEditor) {
			JavaEditor javaEditor = (JavaEditor)part;
			JavaSourceViewer javaSourceViewer = (JavaSourceViewer)javaEditor.getViewer();

			JavaDirtyRegionProcessor processor = fAsYouTypeValidators.get(javaEditor);
			if (processor != null) {
				// Emulate editor closed due to uninstall the old processor
				editorClosed(part);
				Assert.isTrue(null == fAsYouTypeValidators.get(javaEditor), "An old JavaDirtyRegionProcessor is not un-installed on Java Editor instance");
			}

			processor = new JavaDirtyRegionProcessor(javaEditor);
			processor.install(javaSourceViewer);
			processor.setDocument(javaSourceViewer.getDocument());
			processor.setDocumentPartitioning(IJavaPartitions.JAVA_PARTITIONING);
			processor.startReconciling();
			fAsYouTypeValidators.put(javaEditor, processor);
		}
	}

	private void editorClosed(IEditorPart part) {
		if (part instanceof JavaEditor) {
			JavaEditor javaEditor = (JavaEditor)part;

			JavaDirtyRegionProcessor processor = fAsYouTypeValidators.remove(javaEditor);
			if (processor != null) {
				processor.uninstall();
				Assert.isTrue(null == fAsYouTypeValidators.get(javaEditor), "An old JavaDirtyRegionProcessor is not un-installed on Java Editor instance");
			}
		}
	}
}