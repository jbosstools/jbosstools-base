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
import java.util.Collection;

import org.jboss.tools.common.core.jdt.FavoritesClassController;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueEditor;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class JavaFavoritesFieldEditor  extends ExtendedFieldEditor implements IFieldEditor, IPropertyFieldEditor, IValueEditor, PropertyChangeListener {
	
	protected IPropertyEditor propertyEditor;
	protected IValueChangeListener valueChangeListener;
	protected IValueProvider valueProvider;

	protected String filter;
	protected String javaProjectName;

	private Label classListLabel;
	private TableViewer classList;
	private Object[] elements;

//	private ILabelProvider fRenderer;

	public JavaFavoritesFieldEditor() {}
	
	public JavaFavoritesFieldEditor(IWidgetSettings settings) {
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
	}

	public int getNumberOfControls() {
		return 1;
	}

	public Control[] getControls(Composite parent) {
		return new Control[] { getControl(parent) };
	}

	

	protected Control getControl(Composite parent) {
		
		Collection<String> favoritesClasses = FavoritesClassController.getFavoritesClassesList();
		Object[] typeRefs= (Object[])favoritesClasses.toArray(new Object[favoritesClasses.size()]);
		this.elements = typeRefs;
		
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(1, false);
		composite.setLayout(layout);
		layout.horizontalSpacing = 7;
		layout.verticalSpacing = 7;
		layout.marginHeight = 10;
		layout.marginWidth = 10;
		
		// label
		classListLabel = new Label(composite, SWT.NONE);
		classListLabel.setText(EditorMessages.getString("JavaFavoritesChoicerFieldEditor.ClassesList.Label"));
		
		// list
		classList = createList(composite);
		
		return composite;
	}

	protected TableViewer createList(Composite parent) {
		int flags =	SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL | SWT.SINGLE;

		classList =	new TableViewer(parent, flags);

		GridData data = new GridData();
		data.grabExcessVerticalSpace = true;
		data.grabExcessHorizontalSpace = true;
		data.horizontalAlignment = GridData.FILL;
		data.verticalAlignment = GridData.FILL;
		classList.getControl().setLayoutData(data);
		classList.getControl().setFont(parent.getFont());

		classList.setLabelProvider(FavoritesClassController.getLabelProvider());
		classList.setContentProvider(new FavoritesClassesContentProvider());
		classList.setInput("Go!");
		classList.addPostSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				handleSelected(event);
			}
		});
		
		return classList;
	}
	
	protected void handleSelected(SelectionChangedEvent event) {
		IStructuredSelection selection = (IStructuredSelection)classList.getSelection();
		Object obj = selection.getFirstElement();
		if (obj!=null) {
			String selectionClass = (String)obj;
//			String oldValue = (String)valueProvider.getValue();
			PropertyChangeEvent changeEvent = new PropertyChangeEvent(this, IPropertyEditor.VALUE, null, selectionClass);
			valueProvider.removeValueChangeListener(this);
			valueChangeListener.valueChange(changeEvent);
			valueProvider.addValueChangeListener(this);
		}
	}
	
	protected void handleValueProviderValueChange(PropertyChangeEvent event) {
	}
	
	// IValueEditor
	public void setValueChangeListener(IValueChangeListener valueChangeListener) {
		this.valueChangeListener = valueChangeListener;
	}
	
	public void setValueProvider(IValueProvider valueProvider) {
		this.valueProvider = valueProvider;
		valueProvider.addValueChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent event) {
				if (IPropertyEditor.VALUE.equals(event.getPropertyName())) {
					handleValueProviderValueChange(event);
				}
			}
		});
	}
	
	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
	}
	
	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueProvider.addValueChangeListener(this);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
		}
	}

	// java.beans.PropertyChangeListener for valueProvider
	public void propertyChange(PropertyChangeEvent event) {
		// eat events
	}
	
	class FavoritesClassesContentProvider implements IStructuredContentProvider {
		public void dispose() {
		}

		public void inputChanged(Viewer viewer,	Object oldInput,Object newInput) {
		}

		public Object[] getElements(Object inputElement) {
			return elements;
		}

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
