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

import java.util.Map;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnLayoutData;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;

/**
 * @author snjeza
 * 
 */
public class DownloadRuntimeViewerDialog extends Dialog {
	
	private TableViewer viewer;
	private Map<String, DownloadRuntime> downloadRuntimes;

	public DownloadRuntimeViewerDialog(Shell parentShell) {
		super(parentShell);
		setShellStyle(SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER
				| SWT.RESIZE | getDefaultOrientation());
		// FIXME
		downloadRuntimes = RuntimeCoreActivator.getDefault().getDownloadRuntimes();
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		getShell().setText("Runtimes");
		Composite area = (Composite) super.createDialogArea(parent);
		area.setLayout(new GridLayout());
		Composite contents = new Composite(area, SWT.NONE);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
		applyDialogFont(contents);
		initializeDialogUnits(area);

		viewer = new TableViewer(parent, SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.BORDER);
		gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.heightHint = 400;
		gd.widthHint = 700;
		viewer.getTable().setLayoutData(gd);
		
		Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		table.setFont(parent.getFont());
		
		viewer.setContentProvider(new DownloadRuntimesContentProvider());
		
		//String[] columnHeaders = {"Name", "ID", "Version", "URL"};
		String[] columnHeaders = {"Name", "ID", "Version"};
		for (int i = 0; i < columnHeaders.length; i++) {
			TableViewerColumn column = new TableViewerColumn(viewer, SWT.NONE);
			column.setLabelProvider(new DownloadRuntimesLabelProvider(i));
			column.getColumn().setText(columnHeaders[i]);
			column.getColumn().setResizable(true);
			column.getColumn().setMoveable(true);
		}
		
		ColumnLayoutData[] runtimePathsLayouts= {
				new ColumnWeightData(250,250),
				new ColumnWeightData(200,200),
				new ColumnWeightData(150,150),
				
			};
		
		TableLayout layout = new AutoResizeTableLayout(table);
		for (int i = 0; i < runtimePathsLayouts.length; i++) {
			layout.addColumnData(runtimePathsLayouts[i]);
		}
		
		viewer.setInput(downloadRuntimes);
		if (downloadRuntimes.values().size() > 0) {
			viewer.getTable().select(0);
		}
		viewer.getTable().setLayout(layout);
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				validate();
			}
		});
		return area;
	}

	class DownloadRuntimesContentProvider implements IStructuredContentProvider {

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			
		}

		@Override
		public Object[] getElements(Object inputElement) {
			return downloadRuntimes.values().toArray(new DownloadRuntime[0]);
		}
		
		@Override
		public void dispose() {
			
		}
	}
	
	class DownloadRuntimesLabelProvider extends ColumnLabelProvider {

		private int columnIndex;

		public DownloadRuntimesLabelProvider(int i) {
			this.columnIndex = i;
		}

		public String getText(Object element) {
			if (element instanceof DownloadRuntime) {
				DownloadRuntime downloadRuntime = (DownloadRuntime) element;
				switch (columnIndex) {
				case 0:
					return downloadRuntime.getName();
				case 1:
					return downloadRuntime.getId();
				case 2:
					return downloadRuntime.getVersion();
				case 3:
					return downloadRuntime.getUrl();
				}
			}
			return null;
		}
	}

	@Override
	protected void okPressed() {
		ISelection selection = viewer.getSelection();
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection structuredSelection = (IStructuredSelection) selection;
			Object object = structuredSelection.getFirstElement();
			if (object instanceof DownloadRuntime) {
				DownloadRuntime downloadRuntime = (DownloadRuntime) object;
				DownloadRuntimeDialog dialog = new DownloadRuntimeDialog(getShell(), downloadRuntime);
				dialog.open();
			}
		}
		super.okPressed();
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		super.createButtonsForButtonBar(parent);
		validate();
	}

	protected void validate() {
		getButton(IDialogConstants.OK_ID).setEnabled(viewer.getSelection() != null);
	}
	
}
