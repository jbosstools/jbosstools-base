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
package org.jboss.tools.runtime.ui.download;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.resource.ImageDescriptor;
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
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.wst.server.core.TaskModel;
import org.eclipse.wst.server.ui.internal.wizard.TaskWizard;
import org.eclipse.wst.server.ui.internal.wizard.fragment.LicenseWizardFragment;
import org.eclipse.wst.server.ui.wizard.WizardFragment;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeFilter;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.dialogs.AutoResizeTableLayout;

/**
 * @author snjeza
 * 
 */
public class DownloadRuntimeViewerDialog extends TitleAreaDialog {
	
	private TableViewer viewer;
	private Map<String, DownloadRuntime> downloadRuntimes;
	private DownloadRuntimeDialog saveLocationDialog;
	
	public DownloadRuntimeViewerDialog(Shell parentShell) {
		this(parentShell, null);
	}

	public DownloadRuntimeViewerDialog(Shell parentShell, IDownloadRuntimeFilter filter) {
		super(parentShell);
		setShellStyle(SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER
				| SWT.RESIZE | getDefaultOrientation());
		Map<String, DownloadRuntime> allDownloads = RuntimeCoreActivator.getDefault().getDownloadRuntimes();
		if( filter == null )
			downloadRuntimes = RuntimeCoreActivator.getDefault().getDownloadRuntimes();
		else {
			Map<String, DownloadRuntime> filtered = new HashMap<String, DownloadRuntime>();	
			Iterator<String> it = allDownloads.keySet().iterator();
			while(it.hasNext()) {
				String k = it.next();
				DownloadRuntime rt = allDownloads.get(k);
				if( filter.accepts(rt)) {
					filtered.put(k, rt);
				}
			}
			downloadRuntimes = filtered;
		}
		ImageDescriptor desc = RuntimeUIActivator.imageDescriptorFromPlugin(RuntimeUIActivator.PLUGIN_ID,
				"icons/DownloadRuntimeWizBan.png"); //$NON-NLS-1$
		setTitleImage(desc.createImage());
	}

	
	@Override
	protected Control createDialogArea(Composite parent) {
		getShell().setText("Download Runtimes");
		setTitle("Download Runtimes");
		setMessage("Please select a runtime to download and install.");

		Composite area = (Composite) super.createDialogArea(parent);
		Composite contents = new Composite(area, SWT.NONE);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
		applyDialogFont(contents);
		initializeDialogUnits(area);

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
		
		String[] columnHeaders = {"Name", "Version", "URL"};
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
				case 3:
					return downloadRuntime.getId();
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
				if( licenseApproved(downloadRuntime)) {
					saveLocationDialog = new DownloadRuntimeDialog(getShell(), downloadRuntime);
					saveLocationDialog.open();
				} else {
					return;
				}
			}
		}
		super.okPressed();
	}
	
	private boolean licenseApproved(DownloadRuntime downloadRuntime) {

		String license = null;
		try {
			license = downloadRuntime.getLicense(new NullProgressMonitor());
		} catch(CoreException ce) {
			RuntimeUIActivator.log(ce);
		}
		if( license != null ) {
			TaskModel taskModel = new TaskModel();
			taskModel.putObject(LicenseWizardFragment.LICENSE, license);
			TaskWizard wizard2 = new TaskWizard("Download and Install Runtime", new WizardFragment() {
				protected void createChildFragments(List list) {
					list.add(new LicenseWizardFragment());
				}
			}, taskModel);
			
			WizardDialog dialog2 = new WizardDialog(viewer.getTable().getShell(), wizard2);
			if (dialog2.open() == Window.CANCEL) {
				return false;
			}
		}
		return true;
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		super.createButtonsForButtonBar(parent);
		validate();
	}

	protected void validate() {
		getButton(IDialogConstants.OK_ID).setEnabled(viewer.getSelection() != null);
	}
	
	public boolean isDownloading() {
		return saveLocationDialog != null && saveLocationDialog.getReturnCode() == Window.OK; 
	}
}
