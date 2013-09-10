/*************************************************************************************
 * Copyright (c) 2010-2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.internal.wizard;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Map;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnLayoutData;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.jboss.tools.foundation.ui.xpl.taskwizard.IWizardHandle;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.internal.Messages;
import org.jboss.tools.runtime.ui.internal.dialogs.AutoResizeTableLayout;
import org.jboss.tools.runtime.ui.wizard.DownloadRuntimesTaskWizard;

/**
 * 
 * @author snjeza
 *
 */
public class SelectDownloadRuntimeFragment extends WizardFragment {
	private Map<String, DownloadRuntime> downloadRuntimes;
	private TableViewer viewer;
	
	private DownloadRuntime selectedRuntime;
	
	private IWizardHandle handle;
	
	public SelectDownloadRuntimeFragment() {
		this(RuntimeCoreActivator.getDefault().getDownloadRuntimes());
	}
	public SelectDownloadRuntimeFragment(Map<String, DownloadRuntime> downloadRuntimes) {
		this.downloadRuntimes = downloadRuntimes;
	}
	
	@Override
	public boolean hasComposite() {
		return true;
	}


	@Override
	public Composite createComposite(Composite parent, IWizardHandle handle) {
		this.handle = handle;
		getPage().setTitle(Messages.DownloadRuntimesWizardFirstPage_Download_Runtimes);
		getPage().setDescription(Messages.DownloadRuntimesWizardFirstPage_Please_select_a_runtime);
		
		Composite contents = new Composite(parent, SWT.NONE);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
		
		if (downloadRuntimes == null || downloadRuntimes.isEmpty()) {
			new Label(contents, SWT.NONE).setText(Messages.DownloadRuntimesWizardFirstPage_No_available_runtime);
			setComplete(false);
			return contents;
		}
		viewer = new TableViewer(contents, SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL
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
		
		String[] columnHeaders = {Messages.DownloadRuntimesWizardFirstPage_Name, Messages.DownloadRuntimesWizardFirstPage_Version, Messages.DownloadRuntimesWizardFirstPage_URL};
		for (int i = 0; i < columnHeaders.length; i++) {
			TableViewerColumn column = new TableViewerColumn(viewer, SWT.NONE);
			column.setLabelProvider(new DownloadRuntimesLabelProvider(i));
			column.getColumn().setText(columnHeaders[i]);
			column.getColumn().setResizable(true);
			column.getColumn().setMoveable(true);
		}
		
		ColumnLayoutData[] runtimePathsLayouts= {
				new ColumnWeightData(250,250),
				new ColumnWeightData(80,80),
				new ColumnWeightData(250,250),
			};
		
		TableLayout layout = new AutoResizeTableLayout(table);
		for (int i = 0; i < runtimePathsLayouts.length; i++) {
			layout.addColumnData(runtimePathsLayouts[i]);
		}
		
		viewer.setInput(downloadRuntimes);
		if (downloadRuntimes.values().size() > 0) {
			viewer.getTable().select(0);
			selectRuntime(viewer.getSelection());
		}
		viewer.getTable().setLayout(layout);
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				ISelection sel = event.getSelection();
				selectRuntime(sel);
			}
		});
		setComplete(selectedRuntime != null);		
		return contents;
	}

	private void selectRuntime(ISelection sel) {
		selectedRuntime = null;
		setComplete(false);
		if (sel instanceof IStructuredSelection) {
			selectedRuntime = (DownloadRuntime) ((IStructuredSelection) sel).getFirstElement();
			setComplete(selectedRuntime != null);
		}
		getTaskModel().putObject(DownloadRuntimesTaskWizard.DL_RUNTIME_PROP, selectedRuntime);
		handle.update();
	}

	class DownloadRuntimesContentProvider implements IStructuredContentProvider {

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			
		}

		@Override
		public Object[] getElements(Object inputElement) {
			DownloadRuntime[] runtimes = downloadRuntimes.values().toArray(new DownloadRuntime[0]);
			Arrays.sort(runtimes, new Comparator<DownloadRuntime>() {
				
				@Override
				public int compare(DownloadRuntime o1, DownloadRuntime o2) {
					if (o1 == null && o2 == null) {
						return 0;
					}
					if (o1 == null) {
						return 1;
					}
					if (o2 == null) {
						return -1;
					}
					String s1 = o1.getName();
					String s2 = o2.getName();
					if (s1 == null && s2 == null) {
						return 0;
					}
					if (s1 == null) {
						return 1;
					}
					if (s2 == null) {
						return -1;
					}
					return s1.compareTo(s2);
				}
			});
			return runtimes;
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
					return downloadRuntime.getVersion();
				case 2:
					return downloadRuntime.getUrl();
				}
			}
			return null;
		}
	}

}
