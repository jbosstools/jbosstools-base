/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.propertieseditor;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.jboss.tools.common.propertieseditor.bundlemodel.*;

public class BundleEditor extends EditorPart {
	BundleModel bundleModel = new BundleModel();
	BundleLanguagesEditor languages;
	BundleLocaleEditor childrenEditor; 
	private ActivationListener fActivationListener= new ActivationListener();
	
	public BundleEditor() {
		bundleModel.addModifiedListener(new BundleModel.ModificationListener() {
			public void changed() {
				firePropertyChange(PROP_DIRTY);
			}
		});
	}

	public void dispose() {
		super.dispose();
		if(fActivationListener != null && bundleModel != null) {
			IWorkbenchWindow window = getSite().getWorkbenchWindow();
			if(window != null) try {
				window.getPartService().removePartListener(fActivationListener);
				window.getShell().removeShellListener(fActivationListener);
			} catch (Exception e) {
				//ignore
			}
		}
		fActivationListener = null;
		if (childrenEditor!=null) childrenEditor.dispose();
		childrenEditor = null;
		if (bundleModel!=null) {
			bundleModel.dispose();
		}
		bundleModel = null;
		if (languages!=null) languages.dispose();
		languages = null;
	}
	
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		setSite(site);
		setInput(input);
		initializeTitle();
		IWorkbenchWindow window = getSite().getWorkbenchWindow();
		window.getPartService().addPartListener(fActivationListener);
		window.getShell().addShellListener(fActivationListener);
	}
	
	private void initializeTitle() {
		IFile f = getFile();
		String title = null;
		if(f != null) {
			String s = f.getName();
			int i = s.lastIndexOf('.');
			if(i >= 0) s = s.substring(0, i);
			title = "bundle " + s;
		} else {
			title = getEditorInput().getName();
		}
		setPartName(title);
		setContentDescription(title);
	}
	
	private IFile getFile() {
		if(getEditorInput() instanceof IFileEditorInput) {
			return ((IFileEditorInput)getEditorInput()).getFile();
		}
		return null;
	}

	public boolean isDirty() {
		return bundleModel.isModified();
	}

	public void setFocus() {
	}

	public void createPartControl(Composite parent) {
		languages = new BundleLanguagesEditor();
		languages.setBundleModel(bundleModel);
		childrenEditor = new BundleLocaleEditor();
		childrenEditor.setBundleModel(bundleModel);
		IEditorInput input = getEditorInput();
		if(input instanceof IFileEditorInput) {
			IFile f = ((IFileEditorInput)input).getFile();
			bundleModel.setMainFile(f);
			bundleModel.load();
		}
		childrenEditor.setObject(bundleModel.getModelObject());
		Composite p = new Composite(parent, SWT.NONE);
		p.setLayout(new GridLayout()); 
		Control c1 = languages.createControl(p);
		c1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		Control c2 = childrenEditor.createControl(p);
		c2.setLayoutData(new GridData(GridData.FILL_BOTH));
		languages.addListener(childrenEditor);
	}

	public void doSave(IProgressMonitor monitor) {
		bundleModel.save();
	}

	public void doSaveAs() {}

	public void gotoMarker(IMarker marker) {}

	public boolean isSaveAsAllowed() {
		return false;
	}
	
	protected void updateEditableMode() {
		if(childrenEditor != null) childrenEditor.update();
	}

	class ActivationListener extends ShellAdapter implements IPartListener {
		private IWorkbenchPart fActivePart;
		private boolean fIsHandlingActivation= false;
		
		public void partActivated(IWorkbenchPart part) {
			fActivePart= part;
			handleActivation();
		}
	
		public void partBroughtToTop(IWorkbenchPart part) {}
	
		public void partClosed(IWorkbenchPart part) {}
	
		public void partDeactivated(IWorkbenchPart part) {
			fActivePart= null;
		}
	
		public void partOpened(IWorkbenchPart part) {}
	
		public void shellActivated(ShellEvent e) {
			updateEditableMode();
			e.widget.getDisplay().asyncExec(new Runnable() {
				public void run() {
					handleActivation();
				}
			});
		}
		
		private void handleActivation() {
			if (fIsHandlingActivation) return;				
			if (fActivePart != null && fActivePart.getSite() == getSite()) {
				fIsHandlingActivation = true;
				try {
					doSanityCheckState(getEditorInput());
				} finally {
					fIsHandlingActivation= false;
				}
			}
		}
	};

//	private long fModificationStamp= -1;
	private boolean fIsSanityCheckEnabled= true;
	
	protected void enableSanityChecking(boolean enable) {
		synchronized (this) {
			fIsSanityCheckEnabled= enable;
		}
	}

	protected void safelySanityCheckState(IEditorInput input) {
		boolean enabled = false;		
		synchronized (this) {
			enabled = fIsSanityCheckEnabled;
		}		
		if (enabled)
			doSanityCheckState(input);
	}

	protected boolean doSanityCheckState(IEditorInput input) {
		if (input instanceof IFileEditorInput) {
			IFile iFile = ((IFileEditorInput)input).getFile();
			if (iFile == null) return false;
/*
			File f = (iFile.getLocation() == null ? null : iFile.getLocation().toFile());
			if (f == null) return false;
			
			if (fModificationStamp == -1) 
				fModificationStamp= f.lastModified();

			long stamp= f.lastModified();
			if (stamp != fModificationStamp) {
				fModificationStamp= stamp;
				handleEditorInputChanged();
				return true;
			}
*/
		}
		return false;
	}
	
}
