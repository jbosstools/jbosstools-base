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
package org.jboss.tools.common.model.ui.attribute.editor;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.Signature;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.TypeNameMatch;
import org.eclipse.jdt.internal.ui.dialogs.TypeSelectionComponent;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class JavaEclipseChoicerFieldEditor extends ExtendedFieldEditor implements IFieldEditor, IPropertyFieldEditor, PropertyChangeListener {
	protected IPropertyEditor propertyEditor;
	protected IValueChangeListener valueChangeListener;
	protected IValueProvider valueProvider;

	protected TypeSelectionComponent tc;
	protected String filter;
	protected String javaProjectName;

	public JavaEclipseChoicerFieldEditor() {}
	
	public JavaEclipseChoicerFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	protected void adjustForNumColumns(int numColumns) {
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
	}

	protected void doLoad() {
	}

	protected void doLoadDefault() {
	}

	protected void doStore() {
		if(tc != null && tc.getSelection() != null && tc.getSelection().length > 0) {
			valueProvider.setValue(tc.getSelection()[0].getFullyQualifiedName());
		}
	}

	public int getNumberOfControls() {
		return 1;
	}

	public Control[] getControls(Composite parent) {
		return new Control[] { getControl(parent) };
	}

	private IJavaSearchScope getScope() {
		IJavaElement[] elements = new IJavaElement[0];
		if(javaProjectName != null) {
			IPath path = new Path(javaProjectName);
			IResource res = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
			IProject proj = res.getProject();
			IJavaProject jproject = JavaCore.create(proj);
			IPackageFragmentRoot fCurrRoot = jproject.getPackageFragmentRoot(res);
			elements = new IJavaElement[] { fCurrRoot.getJavaProject() };
		} else {
			IProject[] ps = ModelPlugin.getWorkspace().getRoot().getProjects();
			ArrayList<IJavaElement> l = new ArrayList<IJavaElement>();
			for (int i = 0; i < ps.length; i++) {
				if(EclipseResourceUtil.getJavaProject(ps[i]) != null) l.add(JavaCore.create(ps[i]));
			}
			elements = l.toArray(new IJavaElement[0]);
		}
		return SearchEngine.createJavaSearchScope(elements);
	}

	private class TitleLabel implements TypeSelectionComponent.ITitleLabel {
		public void setText(String text) {
		}
	}
	
	protected Control getControl(Composite parent) {
		IJavaSearchScope scope = getScope();
//		String title = EditorMessages.getString("JavaEclipseChoicerFieldEditor.ChooseType.Label");
		String message = EditorMessages.getString("JavaEclipseChoicerFieldEditor.ChooseType.Label");
		if (this.valueProvider!=null) {
			filter = (String)this.valueProvider.getValue(); // string fron text field!
		} else {
			filter = "java.lang.Object";
		}

		tc = new TypeSelectionComponent(parent, 0, message, false, scope, IJavaSearchConstants.CLASS + IJavaSearchConstants.INTERFACE, Signature.getSimpleName(filter), new TitleLabel(), null);
		Composite composite = tc;
		tc.triggerSearch();
		
		GridLayout layout = new GridLayout(1, false);
		composite.setLayout(layout);
		layout.horizontalSpacing = 7;
		layout.verticalSpacing = 7;
		layout.marginHeight = 10;
		layout.marginWidth = 10;

		tc.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(SelectionEvent e) {
				if(hasFocus(tc)) handle(tc.getSelection());
			}
			public void widgetSelected(SelectionEvent e) {
				if(hasFocus(tc)) handle(tc.getSelection());
			}
		});
		return composite;
	}
	
	boolean hasFocus(Composite c) {
		if(c.isFocusControl()) return true;
		Control[] cs = c.getChildren();
		for (int i = 0; i < cs.length; i++) {
			if(cs[i].isFocusControl()) return true;
			if(cs[i] instanceof Composite) {
				if(hasFocus((Composite)cs[i])) return true;
			}
		}
		return false;
	}
	
	private void handle(TypeNameMatch[] selection) {
		if(selection == null || selection.length == 0) {
			this.valueProvider.removeValueChangeListener(this);
			valueChangeListener.valueChange(new PropertyChangeEvent(this, PropertyEditor.VALUE, "", ""));		
			this.valueProvider.addValueChangeListener(this);
		} else {
			this.valueProvider.removeValueChangeListener(this);
			valueChangeListener.valueChange(new PropertyChangeEvent(this, PropertyEditor.VALUE, "", selection[0].getFullyQualifiedName()));		
			this.valueProvider.addValueChangeListener(this);
		}
	}
	
	private boolean localChange = false;

	//never invoked
	void handleSelectionChange(PropertyChangeEvent event) {
		if (!localChange) {
			this.valueProvider.removeValueChangeListener(this);
			valueChangeListener.valueChange(new PropertyChangeEvent(this, PropertyEditor.VALUE, event.getOldValue(), event.getNewValue()));		
			this.valueProvider.addValueChangeListener(this);
		}
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
	}

	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			// GET Java Project Name!
			if (valueProvider instanceof DefaultValueAdapter) {
				if (((DefaultValueAdapter)valueProvider).getModel()!=null) {
					XModelObject xmo = ((DefaultValueAdapter)valueProvider).getModel().getByPath("FileSystems");
					if (xmo!=null) {
						IProject project = (IProject)xmo.getModel().getProperties().get("project");
						if (project!=null) { 
							this.javaProjectName = project.getName();
						}
					}
				}
			}
			valueProvider.addValueChangeListener(this);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
		}
	}

	// java.beans.PropertyChangeListener for valueProvider
	public void propertyChange(PropertyChangeEvent event) {
		/*
		localChange = true;
		if((IPropertyEditor.VALUE.equals(event.getPropertyName()))&&(event!=null)&&(event.getNewValue() instanceof String)) {
			this.filter = (String)event.getNewValue();
		}
		if (ts!=null) {
			try {
				this.ts.setFilter(Signature.getSimpleName(filter));
			} catch (Exception e) {
				// eat exception: widget disposed
			}
		}
		localChange = false;
		*/
	}

	public void cut() {
	}

	public void copy() {
	}

	public void paste() {
	}

	public void delete() {
	}

}
