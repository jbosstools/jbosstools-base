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
package org.jboss.tools.common.editor;

import java.text.MessageFormat;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.*;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.PartInitException;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.DefaultEditorPart;

public abstract class AbstractSectionEditor extends DefaultEditorPart {
	protected SashForm wrapper;
	protected Composite guiControl;
	protected Control control; 
	protected ErrorMode errorMode = new ErrorMode();
	
	public void dispose() {
		if(errorMode == null) return;
		errorMode.dispose();
		errorMode = null;
		disposeGui();
		if(!wrapper.isDisposed()) {
			try {
				wrapper.dispose();
			} catch (SWTException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}
		wrapper = null;
		control = null;
		guiControl = null;
		super.dispose();
	}

	public Control createControl(Composite parent) {
		wrapper = new SashForm(parent, SWT.VERTICAL);
		createErr();
		errorMode.setEnabled(false);
		guiControl = new Composite(wrapper, SWT.NONE);
		GridLayout l = new GridLayout();
		l.marginHeight = 0;
		l.marginWidth = 0;
		guiControl.setLayout(l);
		wrapper.setWeights(new int[]{10, 30});
		return wrapper;	
	}

	public ISelectionProvider getSelectionProvider() {
		return null;
	}
	
	protected void fireGuiModified() {
		if(wrapper!=null)wrapper.update();
		if(wrapper!=null)wrapper.layout();
	}
	
	protected abstract void updateGui();
	
	protected void disposeGui() { 
		if(control != null && !control.isDisposed()) {
				control.dispose();			
		}
		control = null;
	}
	 
	private void createErr() {
		if(errorMode.getControl() != null && !errorMode.getControl().isDisposed()) return;
		Control c = errorMode.createControl(wrapper);
		c.setLayoutData(new GridData(GridData.FILL_BOTH));		
	}
	
	protected XModelObject object = null;	

	public void setObject(XModelObject object, boolean erroneous) {
		this.object = object;
		erroneous = erroneous || isWrongEntity();
		if(object != null) updateGui();
		if(errorMode.isVisible() != erroneous) {
			errorMode.setEnabled(erroneous);
			fireGuiModified();
		}
		if(erroneous) setErroneousObject(object);
	}
	
	public final boolean isWrongEntity() {
		return object != null && isWrongEntity(object.getModelEntity().getName());
	}
	
	protected boolean isWrongEntity(String entity) {
		return false;
	}
	
	public XModelObject getObject() {
		return object;
	}

	private void setErroneousObject(XModelObject object) {
		if(object == null) return;
		String err = object.get("errors"); //$NON-NLS-1$
		if(err == null || err.length() == 0 && isWrongEntity(object.getModelEntity().getName())) {
			err = "Warning: " + 
				"@ @0:0@" + //$NON-NLS-1$
				MessageFormat.format(
						"This editor is not intended for editing {0}. You can use source page, but its coloring may be inadequate.",
						object.getAttributeValue("element type")); //$NON-NLS-1$
		}
		if(err == null) err = ""; //$NON-NLS-1$
		errorMode.update(err);
	}
	
	public void addErrorSelectionListener(ErrorSelectionListener listener) {
		errorMode.addErrorSelectionListener(listener);
	}
	
	public void fireEditorSelected() {}
	
	public void gotoMarker(IMarker marker) {}

	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		super.init(site, input);
		createActions();
		// TODO Create Print Action Here
	}
	
	public void addPropertyListener(IPropertyListener listener) {
	}

	public String getTitle() {
		String s = super.getTitle();
		return s == null ? "" : s; //$NON-NLS-1$
	}

	public Image getTitleImage() {
		return null;
	}

	public String getTitleToolTip() {
		return null;
	}

	public void removePropertyListener(IPropertyListener listener) {
	}

	public void setFocus() {
		if(control == null) return;
		if(!wrapper.isDisposed()) wrapper.setFocus();
	}

	public Object getAdapter(Class adapter) {
		return null;
	}

	public void doSave(IProgressMonitor monitor) {}

	public void doSaveAs() {}

	public boolean isDirty() {
		return false;
	}

	public boolean isSaveAsAllowed() {
		return false;
	}

	public boolean isSaveOnCloseNeeded() {
		return false;
	} 
	
}
