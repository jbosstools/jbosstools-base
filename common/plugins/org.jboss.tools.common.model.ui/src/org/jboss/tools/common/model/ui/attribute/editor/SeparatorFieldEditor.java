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

import java.beans.PropertyChangeListener;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

/**
 * @author AU
 */
public class SeparatorFieldEditor extends ExtendedFieldEditor implements IFieldEditor, IPropertyFieldEditor, IPropertyChangeListener, PropertyChangeListener {
	
	private Label label;

	public SeparatorFieldEditor() {}

	public SeparatorFieldEditor(IWidgetSettings settings) {
		super(settings);
	}
	
	public Control getLabelComposite() {
		return label;
	}

	public Control createLabelComposite(Composite parent) {
		return createLabelControl(parent);
	}

	public Label createLabelControl(Composite parent) {
		if (label == null) {
			label = new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL);
			label.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					label = null;
				}
			});
		} else {
			checkParent(label, parent);
		}
		return label;
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

	public void cut() {
	}

	public void copy() {
	}

	public void paste() {
	}

	public void delete() {
	}

	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent)};
	}

	public void setPropertyEditor(IPropertyEditor propertyEditor) {
	}

	public void propertyChange(PropertyChangeEvent event) {
	}

	public void propertyChange(java.beans.PropertyChangeEvent arg0) {
	}
}
