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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.jboss.tools.common.core.resources.XModelObjectEditorInput;
import org.jboss.tools.common.editor.NullEditorPart;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.IReusableEditor;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.SubActionBars;
import org.eclipse.ui.internal.part.NullEditorInput;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.part.IPageSite;
import org.eclipse.ui.part.Page;
import org.eclipse.ui.part.WorkbenchPart;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.util.IEditorWrapper;

public class EditorPartWrapper extends EditorPart implements IReusableEditor, IEditorWrapper {
	
	public static final String EDITOR_ID = "org.jboss.tools.common.model.ui.editor.EditorPartWrapper"; //$NON-NLS-1$
	
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
				Method method = editorClass.getMethod("gotoMarker",new Class[]{IMarker.class}); //$NON-NLS-1$
				method.setAccessible(true);
				method.invoke(editor,new Object[]{marker});
			} catch(NoSuchMethodException e1) {
				ignore();
			}  catch(IllegalAccessException e2) {
				ignore();
			}  catch(InvocationTargetException e3) {
				ModelUIPlugin.getPluginLog().logError(e3);
			}
		}	 
	}
	
	void ignore() {
		//do nothing
	}

	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		input = XModelObjectEditorInput.checkInput(input);
		if(input instanceof NullEditorInput) {
			entity = ""; //$NON-NLS-1$
			editor = new NullEditorPart();
			editor.init(site, input);
			setSite(site);
			super.setInput(input);
			return;
		}
		entity = computeEntity(input);
		EditorPartWrapperExtension extension = EditorPartWrapperExtension.getInstance();
		EditorPartFactory f = extension.getFactory(entity);
		editor = f.createEditorPart();
		if(editor != null) {
				((WorkbenchPart)editor).setInitializationData(f.getConfigurationElement(), "", null); //$NON-NLS-1$
		}
		editor.init(site, input);
		setSite(site);
		editor.addPropertyListener(new PCL());
		super.setInput(input);
	}

	public void setInput(IEditorInput input) {
    	super.setInput(input);
    	if(editor != null) {
    		editor.dispose();
    		editor = null;
    	}
    	if(parent != null && !parent.isDisposed()) {
    		Control[] cs = parent.getChildren();
    		for (int i = 0; i < cs.length; i++) {
    			if(!cs[i].isDisposed()) cs[i].dispose();
    		}
    	}
    	try {
    		init((IEditorSite)getSite(), input);
    	} catch (PartInitException e) {
    		ModelUIPlugin.getPluginLog().logError(e);
    		return;
    	}
    	if(parent != null && !parent.isDisposed()) {
    		createPartControl(parent);
    	}
    	if(wrapper != null) {
			IContentOutlinePage outline = (IContentOutlinePage)editor.getAdapter(IContentOutlinePage.class);
			wrapper.setOutline(outline);
			if(outline instanceof Page) {
				((SubActionBars)wrapper.getSite().getActionBars()).dispose();
				((Page)outline).init(wrapper.getSite());
				wrapper.getSite().getActionBars().updateActionBars();
				Control[] cs = wrapper.control.getChildren();
				for (int i = 0; i < cs.length; i++) {
					if(!cs[i].isDisposed()) cs[i].dispose();
				}
				outline.createControl(wrapper.control);
	    		outline.getControl().setLayoutData(new GridData(GridData.FILL_BOTH));
				wrapper.control.update();
				wrapper.control.layout();
				wrapper.control.getParent().update();
				wrapper.control.getParent().layout();
				wrapper.control.getParent().getParent().update();
				wrapper.control.getParent().getParent().layout();
				((SubActionBars)wrapper.getSite().getActionBars()).getToolBarManager().update(true);
			}
    	}
    }
	
	static String DEFAULT_ENTITY = "xml"; //$NON-NLS-1$
	
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
	
	Composite parent;

	public void createPartControl(Composite parent) {
		this.parent = parent;
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
	
	COPWrapper wrapper = null;
	
	public Object getAdapter(Class adapter) {
		if(editor == null) {
			return super.getAdapter(adapter);
		}
		if(adapter != null && adapter.isAssignableFrom(IContentOutlinePage.class)) {
			if(wrapper != null) return wrapper;
			wrapper = new COPWrapper();
			IContentOutlinePage outline = (IContentOutlinePage)editor.getAdapter(adapter);
			wrapper.setOutline(outline);
			return wrapper;
		}
		
		return editor.getAdapter(adapter);
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
				EditorPartWrapper.super.setInput(editor.getEditorInput());				
			}
			firePropertyChange(i);
		}
	}

}

class COPWrapper extends ContentOutlinePage {
	IContentOutlinePage outline;
	Composite control;
    private ArrayList<ISelectionChangedListener> selectionChangedListeners = new ArrayList<ISelectionChangedListener>();
    
    public COPWrapper() {}
	
    public void addSelectionChangedListener(ISelectionChangedListener listener) {
    	selectionChangedListeners.add(listener);
        if(outline != null) outline.addSelectionChangedListener(listener);
    }

    public void removeSelectionChangedListener(ISelectionChangedListener listener) {
    	selectionChangedListeners.remove(listener);
        if(outline != null) outline.removeSelectionChangedListener(listener);
    }
    
    public void setOutline(IContentOutlinePage outline) {
    	this.outline = outline;
    	if(outline != null) for (ISelectionChangedListener l: selectionChangedListeners) {
    		outline.addSelectionChangedListener(l);
    	}
    }
    
    public void init(IPageSite pageSite) {
		super.init(pageSite);
		if(outline instanceof Page) {
			((Page)outline).init(pageSite);
		}
	}

	public Control getControl() {
		return control;
	}

    public void createControl(Composite parent) {
    	control = new Composite(parent, SWT.NONE);
    	GridLayout layout = new GridLayout();
    	layout.marginWidth = 0;
    	control.setLayout(layout);
    	control.setLayoutData(new GridData(GridData.FILL_BOTH));
    	
    	if(outline != null) {
    		outline.createControl(control);
    		outline.getControl().setLayoutData(new GridData(GridData.FILL_BOTH));
    	}
    }

    public ISelection getSelection() {
    	return outline != null ? outline.getSelection() : null;
    }
    
    public void selectionChanged(SelectionChangedEvent event) {
    	if(outline instanceof ContentOutlinePage) {
    		((ContentOutlinePage)outline).selectionChanged(event);
    	}
    }

    public void setFocus() {
    	if(outline != null) outline.setFocus();
    }

    public void setSelection(ISelection selection) {
    	if(outline != null) outline.setSelection(selection);
    }
    
    public void dispose() {
    	if(outline != null) outline.dispose();
    	if(control != null) {
    		if(!control.isDisposed()) control.dispose();
    		control = null;
    	}
    	super.dispose();
    }

}
