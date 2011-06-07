/*************************************************************************************
 * Copyright (c) 2010-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.dialogs;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.jboss.tools.runtime.core.model.RuntimePath;

/**
 * @author snjeza
 * 
 */
public class RuntimePathEditingSupport extends EditingSupport {

	private CellEditor editor;
	private int column;

	public RuntimePathEditingSupport(ColumnViewer viewer, int column) {
		super(viewer);
		switch (column) {
		case 0:
			editor = new TextCellEditor(((TableViewer) viewer).getTable());
			break;
		case 1:
			editor = new CheckboxCellEditor(((TableViewer) viewer).getTable());
			break;
		default:
			editor = new TextCellEditor(((TableViewer) viewer).getTable());
		}

		
		this.column = column;
	}


	@Override
	protected boolean canEdit(Object element) {
		if (this.column == 0) {
			return false;
		}
		return true;
	}

	@Override
	protected CellEditor getCellEditor(Object element) {
		return editor;
	}

	@Override
	protected Object getValue(Object element) {
		RuntimePath runtimePath = (RuntimePath) element;
		String value = null;
		switch (this.column) {
		case 0:
			value = runtimePath.getPath();
			if (value == null) {
				value = ""; //$NON-NLS-1$
			}
			return value;
		case 1:
			boolean scan = runtimePath.isScanOnEveryStartup();
			return scan;
		default:
			break;
		}
		return null;

	}

	@Override
	protected void setValue(Object element, Object value) {
		RuntimePath runtimePath = (RuntimePath) element;

		switch (this.column) {
		case 0:
			if (value != null) {
				runtimePath.setPath(value.toString());
			} else {
				runtimePath.setPath(null);
			}
			ISelection selection = getViewer().getSelection();
			getViewer().setSelection(null);
			getViewer().setSelection(selection);
			break;
		case 1:
			runtimePath.setScanOnEveryStartup((Boolean)value);
			break;
		
		default:
			break;
		}

		getViewer().update(element, null);

	}

}
