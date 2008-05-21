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

import org.jboss.tools.common.model.ui.IStructuredChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public abstract class StructuredEditor extends ValueEditor {

	protected IStructuredContentProvider structuredContentProvider;
	protected IStructuredChangeListener structuredChangeListener;

	protected IValueProvider newStructuredElementProvider;
	
	public StructuredEditor() {}

	public StructuredEditor(IWidgetSettings settings) {
		super(settings);
	}

	public void dispose() {
		super.dispose();
		structuredContentProvider = null;
		structuredChangeListener = null;
	}
	
	protected abstract CellEditor createCellEditor(Composite parent);
	protected abstract ExtendedFieldEditor createFieldEditor(Composite parent);
}
