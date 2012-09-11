/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.preferences;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Set;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.PixelConverter;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnLayoutData;
import org.eclipse.jface.viewers.ColumnViewerEditor;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationEvent;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationStrategy;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.FocusCellOwnerDrawHighlighter;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TableViewerEditor;
import org.eclipse.jface.viewers.TableViewerFocusCellManager;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IDownloadRuntimes;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.IRuntimePathChangeListener;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.ui.RuntimeSharedImages;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.dialogs.AutoResizeTableLayout;
import org.jboss.tools.runtime.ui.dialogs.EditRuntimePathDialog;
import org.jboss.tools.runtime.ui.dialogs.RuntimePathEditingSupport;
import org.jboss.tools.runtime.ui.download.DownloadRuntimes;

/**
 * @author snjeza
 * 
 */
public class RuntimePreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	public static String ID = "org.jboss.tools.runtime.preferences.RuntimePreferencePage";
	private RuntimePath[] runtimePaths = new RuntimePath[0];
	private TableViewer runtimePathViewer;
	private RuntimePath runtimePath;
	private Set<IRuntimeDetector> runtimeDetectors;
	private TableViewer detectorViewer;
	private Button searchButton;
	private Button downloadButton;
	private IRuntimePathChangeListener runtimePathChangeListener;

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse
	 * .swt.widgets.Composite)
	 */
	@Override
	protected Control createContents(Composite parent) {
		//noDefaultAndApplyButton();
		initializeDialogUnits(parent);
		
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(1, false);
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		composite.setLayout(layout);
		
		Group pathsGroup = createGroup(composite,1);
		pathsGroup.setText("Description");
		Label pathsDescription = new Label(pathsGroup, SWT.NONE);
		pathsDescription.setText("Each path on this list will be automatically scanned for runtimes when\n" +
				"a new workspace is created or if selected at every Eclipse startup.\n" +
				"Click Edit to configure rules/filters for the search.");
		
		Group pathsTableGroup = createGroup(composite,2);
		pathsTableGroup.setText("Paths");
		runtimePathViewer = createRuntimePathViewer(pathsTableGroup);
		
		Group detectorGroup = createGroup(composite,1);
		detectorGroup.setText("Available runtime detectors");
		detectorViewer = createDetectorViewer(detectorGroup);
				
		Dialog.applyDialogFont(composite);
		return composite;
	}

	private Group createGroup(Composite composite, int column) {
		GridLayout layout;
		Group group = new Group(composite, SWT.NONE);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		group.setLayoutData(gd);
		layout = new GridLayout(column, false);
		group.setLayout(layout);
		return group;
	}
	
	private TableViewer createDetectorViewer(Composite parent) {
		final CheckboxTableViewer tableViewer = CheckboxTableViewer.newCheckList(parent, SWT.BORDER
				| SWT.V_SCROLL | SWT.SINGLE);
		Table table = tableViewer.getTable();
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = 150;
		table.setLayoutData(gd);
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		String[] columnNames = new String[] { "Type", "Link"};
		int[] columnWidths = new int[] { 300, 50};
		
		for (int i = 0; i < columnNames.length; i++) {
			TableColumn tc = new TableColumn(table, SWT.LEFT);
			tc.setText(columnNames[i]);
			tc.setWidth(columnWidths[i]);
		}

		tableViewer.setLabelProvider(new RuntimeDetectorLabelProvider());
		tableViewer.setContentProvider(new RuntimeDetectorContentProvider(runtimeDetectors));
		
		tableViewer.setInput(runtimeDetectors);
		for (IRuntimeDetector detector:runtimeDetectors) {
			tableViewer.setChecked(detector, detector.isEnabled());
		}
		tableViewer.addCheckStateListener(new ICheckStateListener() {
			
			public void checkStateChanged(CheckStateChangedEvent event) {
				IRuntimeDetector detector = (IRuntimeDetector) event.getElement();
				if (detector.isValid()) {
					detector.setEnabled(!detector.isEnabled());
				} else {
					MessageDialog.openWarning(getShell(), "Information", "The '" + detector.getName() + "' detector is invalid.");
					tableViewer.setChecked(detector, false);
				}
				
			}
		});
		for (int i=0; i<runtimeDetectors.size(); i++) {
			TableItem item = table.getItem(i);
			Object data = item.getData();
			if (data instanceof IRuntimeDetector) {
				IRuntimeDetector detector = (IRuntimeDetector) data;
				final String preferenceId = detector.getPreferenceId();
				if (preferenceId != null && preferenceId.trim().length() > 0) {
					Link link = new Link(table, SWT.NONE);
					link.setText("     <a>Link</a>");
					link.setEnabled(detector.isValid());
					TableEditor editor = new TableEditor (table);
					editor.grabHorizontal = editor.grabVertical = true;
					editor.setEditor (link, item, 1);
					link.addSelectionListener(new SelectionAdapter() {
						public void widgetSelected(SelectionEvent e) {
							PreferencesUtil.createPreferenceDialogOn(getShell(),preferenceId, null, null);
						}
					});
				}
			}
		}
		return tableViewer;
	}

	private TableViewer createRuntimePathViewer(Composite parent) {
		final TableViewer viewer = new TableViewer(parent, SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.BORDER);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = 150;
		viewer.getTable().setLayoutData(gd);
		
		Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		table.setFont(parent.getFont());
		
		viewer.setContentProvider(new RuntimePathContentProvider());
		
		String[] columnHeaders = {"Path", "Every start"};
		
		for (int i = 0; i < columnHeaders.length; i++) {
			TableViewerColumn column = new TableViewerColumn(viewer, SWT.NONE);
			column.setLabelProvider(new RuntimePathLabelProvider(i));
			column.getColumn().setText(columnHeaders[i]);
			column.getColumn().setResizable(true);
			column.getColumn().setMoveable(true);
			column.setEditingSupport(new RuntimePathEditingSupport(viewer, i));
		
		}
		
		ColumnLayoutData[] runtimePathsLayouts= {
				new ColumnWeightData(150,150),
				new ColumnWeightData(60,60)
			};
		
		TableLayout layout = new AutoResizeTableLayout(table);
		for (int i = 0; i < runtimePathsLayouts.length; i++) {
			layout.addColumnData(runtimePathsLayouts[i]);
		}
		
		viewer.getTable().setLayout(layout);
		
		configureViewer(viewer);

		
		viewer.setInput(runtimePaths);

		createRuntimePathsButtons(parent, viewer);
		runtimePathChangeListener = new IRuntimePathChangeListener() {
			public void changed() {
				Display.getDefault().asyncExec(new Runnable() {
					public void run() {
						if (runtimePathChangeListener != null) {
							viewer.setInput(RuntimeUIActivator.getRuntimePaths());
						}
					}
				});
				
			}
		};
		RuntimeUIActivator.getDefault().getModel().addRuntimePathChangeListener(runtimePathChangeListener);
		return viewer;
	}

	private void configureViewer(final TableViewer viewer) {
		TableViewerFocusCellManager focusCellManager = new TableViewerFocusCellManager(viewer, new FocusCellOwnerDrawHighlighter(viewer));
		
		ColumnViewerEditorActivationStrategy actSupport = new ColumnViewerEditorActivationStrategy(viewer) {
			protected boolean isEditorActivationEvent(
					ColumnViewerEditorActivationEvent event) {
				ViewerCell cell = viewer.getColumnViewerEditor().getFocusCell();
				if (cell != null && cell.getColumnIndex() == 1) {
					return super.isEditorActivationEvent(event);
				}
				return event.eventType == ColumnViewerEditorActivationEvent.TRAVERSAL
						|| event.eventType == ColumnViewerEditorActivationEvent.MOUSE_DOUBLE_CLICK_SELECTION
						|| (event.eventType == ColumnViewerEditorActivationEvent.KEY_PRESSED && event.keyCode == SWT.CR)
						|| event.eventType == ColumnViewerEditorActivationEvent.PROGRAMMATIC;
			}
		};
		
		TableViewerEditor.create(viewer, focusCellManager, actSupport, ColumnViewerEditor.TABBING_HORIZONTAL
				| ColumnViewerEditor.TABBING_MOVE_TO_ROW_NEIGHBOR
				| ColumnViewerEditor.TABBING_VERTICAL | ColumnViewerEditor.KEYBOARD_ACTIVATION);
	}

	private void createRuntimePathsButtons(Composite parent, final TableViewer viewer) {
		Composite buttonComposite = new Composite(parent, SWT.NONE);
		buttonComposite.setLayout(new GridLayout(1,false));
		buttonComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, false, false));
		
		Button addButton = new Button(buttonComposite, SWT.PUSH);
		addButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		addButton.setText("Add");
		addButton.addSelectionListener(new SelectionAdapter(){
			public void widgetSelected(SelectionEvent e) {
				addPressed();
			}
		});
		
		final Button editButton = new Button(buttonComposite, SWT.PUSH);
		editButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		editButton.setText("Edit...");
		editButton.setEnabled(false);
		
		editButton.addSelectionListener(new SelectionAdapter(){
			public void widgetSelected(SelectionEvent e) {
				editPressed();
			}
		});
		
		final Button removeButton = new Button(buttonComposite, SWT.PUSH);
		removeButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		removeButton.setText("Remove");
		removeButton.setEnabled(false);
		
		removeButton.addSelectionListener(new SelectionAdapter(){
			public void widgetSelected(SelectionEvent e) {
				removedPressed();
			}
		});
		
		searchButton = new Button(buttonComposite, SWT.PUSH);
		searchButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		searchButton.setText("Search...");
		searchButton.setEnabled(runtimePaths.length > 0);
		searchButton.addSelectionListener(new SelectionAdapter(){
			public void widgetSelected(SelectionEvent e) {
				RuntimeUIActivator.launchSearchRuntimePathDialog(getShell(), 
						runtimePaths, true, 15);
			}
		});
		
		downloadButton = new Button(buttonComposite, SWT.PUSH);
		downloadButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		downloadButton.setText("Download...");
		
		final IDownloadRuntimes downloader = getDownloader();
		downloadButton.setEnabled(downloader != null);
		downloadButton.addSelectionListener(new SelectionAdapter(){
		
			public void widgetSelected(SelectionEvent e) {
				HashMap<String, Object> data = new HashMap<String, Object>();
				data.put(DownloadRuntimes.SHELL, getShell());
				downloader.execute(data);
			}
		});
		
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			
			public void selectionChanged(SelectionChangedEvent event) {
				ISelection sel = viewer.getSelection();
				if (sel instanceof IStructuredSelection) {
					IStructuredSelection selection = (IStructuredSelection) sel;
					Object object = selection.getFirstElement();
					editButton.setEnabled(object instanceof RuntimePath);
					removeButton.setEnabled(object instanceof RuntimePath);
				} else {
					editButton.setEnabled(false);
					removeButton.setEnabled(false);
					//searchButton.setEnabled(false);
				}
			}
		});	
	}

	private void addPressed() {
		IDialogSettings dialogSettings = RuntimeUIActivator.getDefault().getDialogSettings();
		String lastUsedPath= dialogSettings.get(RuntimeUIActivator.LASTPATH);
		if (lastUsedPath == null) {
			lastUsedPath= ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString();
		}
		DirectoryDialog dialog = new DirectoryDialog(getShell());
		dialog.setMessage("Add a new path");
		dialog.setFilterPath(lastUsedPath);
		final String path = dialog.open();
		if (path == null) {
			return;
		}
		dialogSettings.put(RuntimeUIActivator.LASTPATH, path);
		RuntimePath runtimePath = new RuntimePath(path);
		boolean exists = Arrays.asList(runtimePaths).contains(runtimePath);
		if (exists) {
			MessageDialog.openInformation(getShell(), "Add Runtime Path", "This runtime path already exists");
			return;
		}
		RuntimeUIActivator.launchSearchRuntimePathDialog(getShell(), 
				new RuntimePath[]{runtimePath}, true, 15);
		configureSearch();
		runtimePathViewer.setInput(runtimePath.getRuntimeDefinitions());
		runtimePathViewer.refresh();
	}
	
	private void removedPressed() {
		ISelection sel = runtimePathViewer.getSelection();
		if (sel instanceof IStructuredSelection) {
			IStructuredSelection selection = (IStructuredSelection) sel;
			Object object = selection.getFirstElement();
			if (object instanceof RuntimePath) {
				ArrayList<RuntimePath> l = new ArrayList<RuntimePath>(Arrays.asList(runtimePaths));
				l.remove(object);
				runtimePaths = (RuntimePath[]) l.toArray(new RuntimePath[l.size()]);
				configureSearch();
				runtimePathViewer.refresh();
			}
		}
	}
	
	private void editPressed() {
		ISelection sel = runtimePathViewer.getSelection();
		if (sel instanceof IStructuredSelection) {
			IStructuredSelection selection = (IStructuredSelection) sel;
			Object object = selection.getFirstElement();
			if (object instanceof RuntimePath) {
				runtimePath = (RuntimePath) object;
				RuntimePath runtimePathClone;
				try {
					runtimePathClone = (RuntimePath) runtimePath.clone();
				} catch (CloneNotSupportedException e1) {
					RuntimeUIActivator.log(e1);
					runtimePathClone = runtimePath;
				}
				EditRuntimePathDialog dialog = new EditRuntimePathDialog(getShell(), runtimePathClone);
				int ok = dialog.open();
				if (ok == Window.OK) {
					if (runtimePath.equals(runtimePathClone)) {
						return;
					}
					if (Arrays.asList(runtimePaths).contains(runtimePathClone)) {
						MessageDialog.openInformation(getShell(), "Edit Runtime Path", "This runtime path already exists");
						return;
					}
					
					ArrayList<RuntimePath> l = new ArrayList<RuntimePath>(Arrays.asList(runtimePaths));
					l.remove(runtimePath);
					runtimePath = runtimePathClone;
					l.add(runtimePath);
					runtimePaths = (RuntimePath[]) l.toArray(new RuntimePath[l.size()]);
					configureSearch();
					runtimePathViewer.refresh();
				}
			}
		}
		// IS THIS NEEDED?!?!
//		if (!getControl().isDisposed()) {
//			RuntimeWorkbenchUtils.refreshPreferencePageUIThread(getShell());
//		}
	}

	
	private IDownloadRuntimes getDownloader() {
		return RuntimeCoreActivator.getDefault().getDownloader();
	}
	
	public void init(IWorkbench workbench) {
		runtimePaths = RuntimeUIActivator.getDefault().getModel().getRuntimePaths();
		runtimeDetectors = RuntimeUIActivator.getDefault().getRuntimeDetectors();
	}
	
	@Override
	public void dispose() {
		RuntimeUIActivator.getDefault().getModel().removeRuntimePathChangeListener(runtimePathChangeListener);
		runtimePathChangeListener = null;
		super.dispose();
	}
	
	/**
	 * Returns a width hint for a button control.
	 * @param button the button
	 * @return the width hint
	 */
	public static int getButtonWidthHint(Button button) {
		button.setFont(JFaceResources.getDialogFont());
		PixelConverter converter= new PixelConverter(button);
		int widthHint= converter.convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
		return Math.max(widthHint, button.computeSize(SWT.DEFAULT, SWT.DEFAULT, true).x);
	}

	/**
	 * Sets width and height hint for the button control.
	 * <b>Note:</b> This is a NOP if the button's layout data is not
	 * an instance of <code>GridData</code>.
	 *
	 * @param button	the button for which to set the dimension hint
	 */
	public static void setButtonDimensionHint(Button button) {
		Assert.isNotNull(button);
		Object gd= button.getLayoutData();
		if (gd instanceof GridData) {
			((GridData)gd).widthHint= getButtonWidthHint(button);
			((GridData)gd).horizontalAlignment = GridData.FILL;
		}
	}
	
	class RuntimePathContentProvider implements IStructuredContentProvider {
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}
		public Object[] getElements(Object inputElement) {
			return runtimePaths;
		}
		public void dispose() {
		}
	}
	
	public static class RuntimePathLabelProvider extends ColumnLabelProvider {

		private int columnIndex;

		public RuntimePathLabelProvider(int i) {
			this.columnIndex = i;
		}

		public String getText(Object element) {
			if (element instanceof RuntimePath) {
				RuntimePath runtimePath = (RuntimePath) element;
				switch (columnIndex) {
				case 0:
					return runtimePath.getPath();
				}
			}
			return null;
		}

		@Override
		public Image getImage(Object element) {
			if (element == null) {
				return null;
			}
			RuntimePath runtimePath = (RuntimePath) element;
			if (columnIndex == 1) {
				if( runtimePath.isScanOnEveryStartup()) 
					return RuntimeSharedImages.getImage(RuntimeSharedImages.CHECKBOX_ON_KEY);
				else
					return RuntimeSharedImages.getImage(RuntimeSharedImages.CHECKBOX_OFF_KEY);
			}
			if (columnIndex == 0) {
				String path = runtimePath.getPath();
				if (path == null || ! (new File(path).isDirectory())) {
					return RuntimeSharedImages.getImage(RuntimeSharedImages.ERROR_KEY);
				}
			}
			return null;
		}
	}
	
	private static class RuntimeDetectorContentProvider implements IStructuredContentProvider {

		private Set<IRuntimeDetector> detectors;

		public RuntimeDetectorContentProvider(Set<IRuntimeDetector> detectors) {
			this.detectors = detectors;
		}
		
		public Object[] getElements(Object inputElement) {
			return detectors.toArray();
		}

		public void dispose() {
			
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			detectors = (Set<IRuntimeDetector>) newInput;
		}
	}
	
	private static class RuntimeDetectorLabelProvider extends LabelProvider implements
			ITableLabelProvider {

		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

		public String getColumnText(Object element, int columnIndex) {
			if (element instanceof IRuntimeDetector) {
				IRuntimeDetector detector = (IRuntimeDetector) element;
				if (columnIndex == 0) {
					return detector.getName();
				}
			}
			return null;
		}
	}

	@Override
	protected void performApply() {
		RuntimeUIActivator.getDefault().saveRuntimePreferences();
	}

	@Override
	protected void performDefaults() {
		runtimePaths = RuntimeUIActivator.getDefault().getModel().getRuntimePaths();
		runtimeDetectors = RuntimeUIActivator.getDefault().getRuntimeDetectors();
		runtimePathViewer.setInput(runtimePaths);
		detectorViewer.setInput(runtimeDetectors);
		super.performDefaults();
	}

	@Override
	public boolean performOk() {
		RuntimeUIActivator.getDefault().getModel().setRuntimePaths(runtimePaths);
		RuntimeUIActivator.getDefault().saveRuntimePreferences();
		return super.performOk();
	}

	private void configureSearch() {
		if (searchButton != null) {
			searchButton.setEnabled(runtimePaths.length > 0);
		}
	}

}
