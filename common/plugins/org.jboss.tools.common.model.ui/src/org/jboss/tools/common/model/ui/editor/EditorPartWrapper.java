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
package org.jboss.tools.common.model.ui.editor;

import java.lang.reflect.Method;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.common.core.resources.XModelObjectEditorInput;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.IReusableEditor;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.part.WorkbenchPart;

import org.jboss.tools.common.model.XModelObject;
//import org.jboss.tools.common.text.xml.internal.ui.xmleditor.XmlEditor;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.text.ext.IEditorWrapper;

public class EditorPartWrapper extends EditorPart implements IReusableEditor, IEditorWrapper {
	IEditorPart editor;	
	String entity = null;

	public IEditorInput getEditorInput() {
		return super.getEditorInput();
	}

	public IEditorSite getEditorSite() {
		return super.getEditorSite();
	}

	public void gotoMarker(IMarker marker) {
		if(editor != null) {
			try {
				Class editorClass = editor.getClass();
				Method method = editorClass.getMethod("gotoMarker",new Class[]{IMarker.class});
				method.invoke(editor,new Object[]{marker});
			} catch(Exception ex) {
				// Eat exception
			}	 
		}	 
	}

	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		input = XModelObjectEditorInput.checkInput(input);
		entity = computeEntity(input);
		EditorPartWrapperExtension extension = EditorPartWrapperExtension.getInstance();
		EditorPartFactory f = extension.getFactory(entity);
		editor = f.createEditorPart();
////		if(editor == null) editor = new XmlEditor();
		if(editor != null) {
			try {
				((WorkbenchPart)editor).setInitializationData(f.getConfigurationElement(), "", null);
			} catch (Exception e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}
		editor.init(site, input);
		setSite(site);
		editor.addPropertyListener(new PCL());
		setInput(input);
	}
	
    public void setInput(IEditorInput input) {
    	super.setInput(input);
    }
	
	static String DEFAULT_ENTITY = "xml";
	
	private String computeEntity(IEditorInput input) {
		if(!(input instanceof IModelObjectEditorInput)) return DEFAULT_ENTITY; 
		IModelObjectEditorInput i = (IModelObjectEditorInput)input;
		XModelObject o = i.getXModelObject();
		if(o == null) return DEFAULT_ENTITY;
		EditorPartWrapperExtension extension = EditorPartWrapperExtension.getInstance();
		EditorPartFactory f = extension.getFactory(o.getModelEntity().getName());
		if(f == null) return DEFAULT_ENTITY;
		return o.getModelEntity().getName();
	}

	public void addPropertyListener(IPropertyListener l) {
		super.addPropertyListener(l);
		if(editor != null) editor.addPropertyListener(l);
	}

	public void createPartControl(Composite parent) {
		if(editor != null) editor.createPartControl(parent);
		parent.layout();
	}

	public void dispose() {
		if(editor != null) {
			editor.dispose();
			editor = null;
		}
		super.dispose(); 
	}

	public IWorkbenchPartSite getSite() {
		return (editor == null   || true    ) ? super.getSite() : editor.getSite();
	}

	public String getTitle() {
		if(editor != null && !super.getTitle().equals(editor.getTitle())) {
			String title = editor.getTitle();
			setPartName(title);
		} 
		return super.getTitle();
	}

	public Image getTitleImage() {
		return (editor == null) ? super.getTitleImage() : editor.getTitleImage();
	}

	public String getTitleToolTip() {
		return (editor == null) ? super.getTitleToolTip() : editor.getTitleToolTip();
	}

	public void removePropertyListener(IPropertyListener l) {
		super.removePropertyListener(l);
		if(editor != null) editor.removePropertyListener(l);
	}

	public void setFocus() {
		if(editor != null) editor.setFocus();
	}

	public Object getAdapter(Class adapter) {
		return (editor == null) ? super.getAdapter(adapter) : editor.getAdapter(adapter);
	}

	public void doSave(IProgressMonitor monitor) {
		if(editor != null) editor.doSave(monitor);
	}

	public void doSaveAs() {
		if(editor != null) editor.doSaveAs();
	}

	public boolean isDirty() {
		return (editor != null && editor.isDirty());
	}

	public boolean isSaveAsAllowed() {
		return (editor != null && editor.isSaveAsAllowed());
	}

	public boolean isSaveOnCloseNeeded() {
		return (editor != null && editor.isSaveOnCloseNeeded());
	}
	
	public IEditorPart getEditor() {
		return editor;
	}
	
	public String getEntity() {
		return entity;
	}
	
	class PCL implements IPropertyListener {
		public void propertyChanged(Object source, int i) {
			if(i == IEditorPart.PROP_INPUT && getEditorInput() != editor.getEditorInput()) {
				setInput(editor.getEditorInput());				
			}
			firePropertyChange(i);
		}
	}

}
