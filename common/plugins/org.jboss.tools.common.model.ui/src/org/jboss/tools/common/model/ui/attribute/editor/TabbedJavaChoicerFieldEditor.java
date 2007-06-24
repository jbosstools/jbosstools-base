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

import org.jboss.tools.common.core.jdt.FavoritesClassController;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class TabbedJavaChoicerFieldEditor extends ExtendedFieldEditor implements IPropertyFieldEditor {
	
	private static final String LAST_CHOICER = "TabbedJavaChoicerFieldEditor.LastChoicer";
	
	protected IPropertyEditor propertyEditor;
	protected TabFolder tabbedPane;
	protected int selectedTab = 0;
	
	protected JavaChoicerFieldEditor classicEditor;
	protected JavaEclipseChoicerFieldEditor eclipseEditor;
	protected JavaFavoritesFieldEditor favoritesEditor;

	public TabbedJavaChoicerFieldEditor() {}
		
	public TabbedJavaChoicerFieldEditor(IWidgetSettings settings) {
		super(settings);
		classicEditor = new JavaChoicerFieldEditor(settings);
		eclipseEditor = new JavaEclipseChoicerFieldEditor(settings);
		favoritesEditor = new JavaFavoritesFieldEditor(settings);
	}

	protected void adjustForNumColumns(int numColumns) {
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		Control control = createTabbedPane(parent);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = numColumns;
		control.setLayoutData(gd);
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

	protected Composite createTabbedPane(Composite parent) {
		GridData gd;
		
		Composite composite = new Composite(parent, SWT.NONE);
		composite.setBackground(new Color(null,128,128,0));
		GridLayout layout = new GridLayout(1, false);
		composite.setLayout(layout);
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = 0;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		gd = new GridData(GridData.FILL_BOTH);
		composite.setLayoutData(gd);

		tabbedPane = new TabFolder(composite, SWT.NONE);
		gd = new GridData(GridData.FILL_BOTH);
		tabbedPane.setLayoutData(gd);
		// classic
		TabItem classicChooser = new TabItem(tabbedPane, SWT.NONE);
		classicChooser.setControl(createClassicChooser(tabbedPane));
		classicChooser.setText(EditorMessages.getString("TabbedJavaChoicerFieldEditor.Tab1.Label"));
		// eclipse
		TabItem eclipseChooser = new TabItem(tabbedPane, SWT.NONE);
		eclipseChooser.setControl(createEclipseChooser(tabbedPane));
		eclipseChooser.setText(EditorMessages.getString("TabbedJavaChoicerFieldEditor.Tab2.Label"));
		// favorites
		TabItem favoritesChooser = new TabItem(tabbedPane, SWT.NONE);
		favoritesChooser.setControl(createFavoritesChooser(tabbedPane));
		favoritesChooser.setText(EditorMessages.getString("TabbedJavaChoicerFieldEditor.Tab3.Label"));
		
		int lastIndex = ModelUIPlugin.getDefault().getPreferenceStore().getInt(LAST_CHOICER);
		selectedTab = lastIndex;
		tabbedPane.setSelection(lastIndex);
		
		tabbedPane.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				handleTabbedPaneSelection();
			}
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		
		return composite; 
	}
	
	private void handleTabbedPaneSelection() {
		selectedTab = tabbedPane.getSelectionIndex();
		tabbedPane.getItem(selectedTab).getControl().setFocus();
		ModelUIPlugin.getDefault().getPreferenceStore().setValue(LAST_CHOICER,selectedTab);
	}

	protected Control createClassicChooser(Composite parent) {
		return classicEditor.getControls(parent)[0];
	}
	protected Control createEclipseChooser(Composite parent) {
		return eclipseEditor.getControls(parent)[0];
	}
	protected Control createFavoritesChooser(Composite parent) {
		return favoritesEditor.getControls(parent)[0];
	}

	//IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		classicEditor.setPropertyEditor(propertyEditor);
		eclipseEditor.setPropertyEditor(propertyEditor);
		favoritesEditor.setPropertyEditor(propertyEditor);
	}

	public void store() {
		super.store();
		if (selectedTab==1)	eclipseEditor.doStore(); // bugfix 8136 && 8155
		IValueProvider valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
		String selectedClass = (String)valueProvider.getValue();
		FavoritesClassController.push(selectedClass);
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
		if (classicEditor!=null) classicEditor.setEnabled(enabled);		
		if (eclipseEditor!=null) eclipseEditor.setEnabled(enabled);		
		if (favoritesEditor!=null) favoritesEditor.setEnabled(enabled);		
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
