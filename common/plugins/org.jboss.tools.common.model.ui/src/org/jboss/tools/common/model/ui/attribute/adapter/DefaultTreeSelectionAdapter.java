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
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.beans.PropertyChangeEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;

import org.eclipse.core.runtime.IAdaptable;
import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.ModelUIMessages;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.editor.PropertyEditor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.util.Assert;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XFilteredTree;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.actions.IActionProvider;

public class DefaultTreeSelectionAdapter extends DefaultValueAdapter implements IAdaptable, ISelectionProvider, ISelectionChangedListener {	

	private static final String STRING_BUTTON_ACTION = "Label.Selected"; //$NON-NLS-1$
	private static final String STRING_BUTTON_XACTION = "linkAction"; //$NON-NLS-1$

	protected ILabelProvider labelProvider;
	protected ITreeContentProvider treeContentProvider;

	public DefaultTreeSelectionAdapter() {}

	public void dispose() {
		super.dispose();
		if (labelProvider!=null) labelProvider.dispose();
		labelProvider = null;
		if (treeContentProvider!=null) treeContentProvider.dispose();
		treeContentProvider = null;
		if (actionProvider!=null) actionProvider.dispose();
		actionProvider = null;
	}

	// IAdaptable
	public Object getAdapter(Class adapter) {
		if (adapter == IValueProvider.class) {
			return this;
		}
		if (adapter == IValueChangeListener.class) {
			return this;
		}
		if (adapter == IAttributeErrorProvider.class) return this;
		if (adapter == ISelectionProvider.class) {
			return this;
		}
		if (adapter == ISelectionChangedListener.class) {
			return this;
		}
		if (adapter == ILabelProvider.class) {
			if (this.labelProvider==null) {
				this.labelProvider = new DefaultXModelObjectLabelProvider();
			}
			return this.labelProvider;
		}
		if (adapter == ITreeContentProvider.class) {
			return getTreeContentProvider();
		}
		if (adapter == IActionProvider.class) {
			return getActionProvider();
		}

		if(adapter == IContentAssistProcessor.class) {
			//deprecated, replaced by AttributeContentProposalProviderFactory
			DefaultTreeSelectionContentAssistProcessor processor = new DefaultTreeSelectionContentAssistProcessor();
			ITreeContentProvider tree = getTreeContentProvider();
			if(tree instanceof DefaultXAttributeTreeContentProvider) {
				processor.setTreeProvider((DefaultXAttributeTreeContentProvider)tree);
				return processor;
			} else {
				return null;
			}
		}

		Assert.isTrue(true, "DefaultTreeSelectionAdapter instance itself cannot provide adapter for "+adapter.getName()); //$NON-NLS-1$
		return null;
	}

	ArrayList<ISelectionChangedListener> selectionChangeListeners = new ArrayList<ISelectionChangedListener>();

	// ISelectionProvider
	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		selectionChangeListeners.add(listener);
	}
	public ISelection getSelection() {
		Object v = getValue();
		Object object = (v == null) ? null : getObjectByPath(v.toString());
		return object == null ? new StructuredSelection() : new StructuredSelection(object);
	}
	
	public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		selectionChangeListeners.remove(listener);
	}
	
	public void setSelection(ISelection selection) {}

	// ISelectionChangedListener
	public void selectionChanged(SelectionChangedEvent event) {
		StructuredSelection selection = (StructuredSelection)event.getSelection();
		Iterator i = selection.iterator();
		while (i.hasNext()) {
			XModelObject object = (XModelObject)i.next();
			if (getFilteredTree()==null) return;
			if(!getFilteredTree().isSelectable(object)) continue;
			this.valueChange(new PropertyChangeEvent(this, PropertyEditor.VALUE, null, getPathByObject(object)));
			if (labelAction!=null) {
				labelAction.setXModelObject(object);
			}
		}
	}	
	
	private Object getObjectByPath(String path) {
		if (getFilteredTree()!=null) {
			return getFilteredTree().find(path);
		}
		return null;
	}
	
	private String getPathByObject(Object object) {
		return getFilteredTree().getValue((XModelObject)object);
	}
	
	private XFilteredTree getFilteredTree() {
		if (getTreeContentProvider() instanceof DefaultXAttributeTreeContentProvider) return ((DefaultXAttributeTreeContentProvider)getTreeContentProvider()).getFilteredTree();
		return null;
	}
	
	// IActionProviderImplementation

	private ActionProvider actionProvider;
	private XActionWrapper labelAction;

	private ActionProvider getActionProvider() {
		if (this.actionProvider==null) this.actionProvider = new ActionProvider(); 
		return this.actionProvider;
	}
	
	class XActionWrapper extends Action {
		
		private XAction xaction = null;
		
		public XActionWrapper(XAction xaction) {
			this.xaction = xaction;		
		}
		
		public void setXModelObject(XModelObject xmo) {
			if (xmo != null && xaction != null) {
//				this.setEnabled(xaction.isEnabled(xmo));
				this.setEnabled(Boolean.TRUE.booleanValue());
			} else {
				this.setEnabled(Boolean.FALSE.booleanValue());
			}
		}
		
		public void run() {
			if (xaction != null) {
				if(xaction.isEnabled(getModelObject())) {
					XActionInvoker.invoke(xaction.getPath(), modelObject, new Properties());
				} else {
					Shell shell = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
					MessageDialog.openWarning(shell, "Warning", "Resource does not exist.");
				}
			}
		}
	}
	
	class ActionProvider implements IActionProvider {
		private HashMap<String,IAction> actions = new HashMap<String,IAction>();

		public IAction getAction(String actionName) {
			return actions.get(actionName);
		}

		public IAction[] getActions() {
			return actions.values().toArray(new IAction[actions.values().size()]);
		}
		
		public void putAction(String actionAlias, IAction action) {
			actions.put(actionAlias, action);
		}
		
		public void dispose() {
			if (actions!=null) actions.clear();
			actions = null;
			
		}
		public void update(ISelection selection) {
//			if(actions != null) {
//				for (IAction a: actions.values()) {
//					if(a instanceof XActionWrapper) {
//						((XActionWrapper)a).setXModelObject(getModelObject());
//					}
//				}
//			}
			//not implemented
		}
	}

	public ITreeContentProvider getTreeContentProvider() {
		if (this.treeContentProvider==null) {
			DefaultXAttributeTreeContentProvider treeContentProvider = new DefaultXAttributeTreeContentProvider(attribute, model, modelObject);
			this.treeContentProvider = treeContentProvider;
		}
		return this.treeContentProvider;
	}
	
	private void initActions() {
		treeContentProvider = getTreeContentProvider();
		if (treeContentProvider instanceof DefaultXAttributeTreeContentProvider) {
			String linkActionName = ((DefaultXAttributeTreeContentProvider)treeContentProvider).getProperties().getProperty(STRING_BUTTON_XACTION); 
			if (linkActionName!=null) {
				XAction xAction = XActionInvoker.getAction(linkActionName, modelObject);
				if (xAction!=null) {
					XActionWrapper linkAction = new XActionWrapper(xAction);
//Let it always be enabled, or we need 
//good update not only depending on value but on workspace resources.
//					XModelObject object = (XModelObject)getObjectByPath(this.getStringValue(Boolean.TRUE.booleanValue()));
//					linkAction.setXModelObject(object);
				linkAction.setEnabled(true);
					getActionProvider().putAction(STRING_BUTTON_ACTION, linkAction);
				}
			}
		}
	}

	/**
	 * @see org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter#setAttribute(org.jboss.tools.common.meta.XAttribute)
	 */
	public void setAttribute(XAttribute attribute) {
		super.setAttribute(attribute);
		if (modelObject!=null && attribute!=null) initActions();
	}

	/**
	 * @see org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter#setModelObject(org.jboss.tools.common.model.XModelObject)
	 */
	public void setModelObject(XModelObject object) {
		super.setModelObject(object);
		if (modelObject!=null && attribute!=null) initActions();
	}

}
