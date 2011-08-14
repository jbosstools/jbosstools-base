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
package org.jboss.tools.common.jdt.debug.ui.preferences;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;

/**
 * @author snjeza
 * 
 */
public class RemoteDebugEditingSupport extends EditingSupport {

	private CellEditor editor;
	private int column;

	public RemoteDebugEditingSupport(ColumnViewer viewer, int column) {
		super(viewer);
		switch (column) {
		case 1:
			editor = new TextCellEditor(((TableViewer) viewer).getTable());
			break;
		case 2:
			editor = new TextCellEditor(((TableViewer) viewer).getTable());
			break;
		case 3:
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
		RemoteDebug remoteDebug = (RemoteDebug) element;
		String value = null;
		switch (this.column) {
		case 0:
			value = remoteDebug.getKey(true);
			if (value == null) {
				value = ""; //$NON-NLS-1$
			}
			return value;
		case 1:
			value = remoteDebug.getDescription();
			if (value == null) {
				value = ""; //$NON-NLS-1$
			}
			return value;
		case 2:
			value = remoteDebug.getPort();
			if (value == null) {
				value = ""; //$NON-NLS-1$
			}
			return value;
		case 3:
			boolean show = remoteDebug.isShow();
			return show;
		default:
			break;
		}
		return null;

	}

	@Override
	protected void setValue(Object element, Object value) {
		RemoteDebug remoteDebug = (RemoteDebug) element;
		switch (this.column) {
		case 1:
			if (value != null) {
				remoteDebug.setDescription(value.toString());
			} else {
				remoteDebug.setDescription(null);
			}
			ISelection selection = getViewer().getSelection();
			getViewer().setSelection(null);
			getViewer().setSelection(selection);
			break;
		case 2:
			if (value != null) {
				remoteDebug.setPort(value.toString());
			} else {
				remoteDebug.setPort(null);
			}
			selection = getViewer().getSelection();
			getViewer().setSelection(null);
			getViewer().setSelection(selection);
			break;
		case 3:
			remoteDebug.setShow((Boolean)value);
			break;
		
		default:
			break;
		}

		getViewer().update(element, null);

	}

}
