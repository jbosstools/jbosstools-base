package org.jboss.tools.common.jdt.debug.ui.preferences;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnLayoutData;
import org.eclipse.jface.viewers.ColumnViewerEditor;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationEvent;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationStrategy;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.FocusCellOwnerDrawHighlighter;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TableViewerEditor;
import org.eclipse.jface.viewers.TableViewerFocusCellManager;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.jboss.tools.common.jdt.debug.ui.Messages;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;

public class RemoteDebugPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

//	private Image checkboxOn;
//	private Image checkboxOff;
//	private Image errorIcon;
//	private RemoteDebug[] remoteDebugs;
//	private TableViewer viewer;
//	private Button discoverButton;
	private Button autoConnectButton;

	@Override
	public void init(IWorkbench workbench) {
//		remoteDebugs = RemoteDebugUIActivator.getDefault().getRemoteDebugs();
//		checkboxOn = RemoteDebugUIActivator.imageDescriptorFromPlugin(RemoteDebugUIActivator.PLUGIN_ID, "/icons/xpl/complete_tsk.gif").createImage();
//		checkboxOff = RemoteDebugUIActivator.imageDescriptorFromPlugin(RemoteDebugUIActivator.PLUGIN_ID, "/icons/xpl/incomplete_tsk.gif").createImage();
//		errorIcon = RemoteDebugUIActivator.imageDescriptorFromPlugin(RemoteDebugUIActivator.PLUGIN_ID, "/icons/xpl/error_tsk.gif").createImage();
	}

	@Override
	protected Control createContents(Composite parent) {
		initializeDialogUnits(parent);
		
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(1, false);
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		composite.setLayout(layout);
		
//		Group group = new Group(composite, SWT.NONE);
//		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
//		group.setLayoutData(gd);
//		layout = new GridLayout(1, false);
//		group.setLayout(layout);
//		group.setText("Key Bindings");
//		
//		viewer = new TableViewer(group, SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL
//				| SWT.V_SCROLL | SWT.BORDER);
//		gd = new GridData(GridData.FILL_BOTH);
//		gd.heightHint = 250;
//		viewer.getTable().setLayoutData(gd);
//		
//		Table table = viewer.getTable();
//		table.setHeaderVisible(true);
//		table.setLinesVisible(true);
//		table.setFont(parent.getFont());
//		
//		viewer.setContentProvider(new RemoteDebugContentProvider());
//		
//		String[] columnHeaders = {"Key", "Description", "Port", "Show"};
//		
//		for (int i = 0; i < columnHeaders.length; i++) {
//			TableViewerColumn column = new TableViewerColumn(viewer, SWT.NONE);
//			column.setLabelProvider(new RemoteDebugLabelProvider(i));
//			column.getColumn().setText(columnHeaders[i]);
//			column.getColumn().setResizable(true);
//			column.getColumn().setMoveable(true);
//			column.setEditingSupport(new RemoteDebugEditingSupport(viewer, i));
//		
//		}
//		
//		ColumnLayoutData[] remoteDebugLayouts= {
//				new ColumnWeightData(150,150),
//				new ColumnWeightData(250,250),
//				new ColumnWeightData(80,80),
//				new ColumnWeightData(60,60),
//			};
//		
//		TableLayout tableLayout = new AutoResizeTableLayout(table);
//		for (int i = 0; i < remoteDebugLayouts.length; i++) {
//			tableLayout.addColumnData(remoteDebugLayouts[i]);
//		}
//		
//		viewer.getTable().setLayout(tableLayout);
//		
//		TableViewerFocusCellManager focusCellManager = new TableViewerFocusCellManager(viewer, new FocusCellOwnerDrawHighlighter(viewer));
//		
//		ColumnViewerEditorActivationStrategy actSupport = new ColumnViewerEditorActivationStrategy(viewer) {
//			protected boolean isEditorActivationEvent(
//					ColumnViewerEditorActivationEvent event) {
//				ViewerCell cell = viewer.getColumnViewerEditor().getFocusCell();
//				if (cell != null && cell.getColumnIndex() == 1) {
//					return super.isEditorActivationEvent(event);
//				}
//				return event.eventType == ColumnViewerEditorActivationEvent.TRAVERSAL
//						|| event.eventType == ColumnViewerEditorActivationEvent.MOUSE_DOUBLE_CLICK_SELECTION
//						|| (event.eventType == ColumnViewerEditorActivationEvent.KEY_PRESSED && event.keyCode == SWT.CR)
//						|| event.eventType == ColumnViewerEditorActivationEvent.PROGRAMMATIC;
//			}
//		};
//		
//		TableViewerEditor.create(viewer, focusCellManager, actSupport, ColumnViewerEditor.TABBING_HORIZONTAL
//				| ColumnViewerEditor.TABBING_MOVE_TO_ROW_NEIGHBOR
//				| ColumnViewerEditor.TABBING_VERTICAL | ColumnViewerEditor.KEYBOARD_ACTIVATION);
//		
//		viewer.setInput(remoteDebugs);
//		
//		discoverButton = new Button(composite, SWT.CHECK);
//		discoverButton.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
//		discoverButton.setSelection(RemoteDebugUIActivator.getDefault().isDiscoverRemoteApplication());
//		discoverButton.setText(Messages.Discover_Remote_Applications);
//		
//		createNoteComposite(composite.getFont(), composite, "Note:", 
//				"It may take some time to discover remote applications.");
		
		autoConnectButton = new Button(composite, SWT.CHECK);
		autoConnectButton.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
		autoConnectButton.setSelection(RemoteDebugUIActivator.getDefault().isAutoConnect());
		autoConnectButton.setText("Automatically connect if only one application found");
		
		return composite;
	}

	class RemoteDebugContentProvider implements IStructuredContentProvider {

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			
		}

		@Override
		public Object[] getElements(Object inputElement) {
			//return remoteDebugs;
			return new Object[0];
		}
		
		@Override
		public void dispose() {
			
		}

	}
	
	class RemoteDebugLabelProvider extends ColumnLabelProvider {

		private int columnIndex;

		public RemoteDebugLabelProvider(int i) {
			this.columnIndex = i;
		}

		public String getText(Object element) {
			if (element instanceof RemoteDebug) {
				RemoteDebug remoteDebug = (RemoteDebug) element;
				switch (columnIndex) {
				case 0:
					return remoteDebug.getKey(true);
				case 1:
					return remoteDebug.getDescription();
				case 2:
					return remoteDebug.getPort();
				}
			}
			return null;
		}

		@Override
		public Image getImage(Object element) {
			if (element == null) {
				return null;
			}
			RemoteDebug remoteDebug = (RemoteDebug) element;
//			if (columnIndex == 0) {
//				return remoteDebug.isValid() ? null : errorIcon;
//			}
//			if (columnIndex == 3) {
//				return remoteDebug.isShow() ? checkboxOn : checkboxOff;
//			}
			return null;
		}
	}

	@Override
	public void dispose() {
//		if (checkboxOff != null) {
//			checkboxOff.dispose();
//		}
//		if (checkboxOn != null) {
//			checkboxOn.dispose();
//		}
//		if (errorIcon != null) {
//			errorIcon.dispose();
//		}
		super.dispose();
	}
	
	@Override
	protected void performApply() {
		IEclipsePreferences preferences = RemoteDebugUIActivator.getDefault().getPreferences();
//		int keys = RemoteDebugUIActivator.KEYS;
//		for (int i = 0; i < keys; i++) {
//			preferences.put(RemoteDebugUIActivator.getDescriptionPreferenceName(i), remoteDebugs[i].getDescription());
//			preferences.put(RemoteDebugUIActivator.getPortPreferenceName(i), remoteDebugs[i].getPort());
//			preferences.putBoolean(RemoteDebugUIActivator.getShowPreferenceName(i), remoteDebugs[i].isShow());
//		}
//		preferences.putBoolean(RemoteDebugUIActivator.DISCOVER_REMOTE_APPLICATION, discoverButton.getSelection());
		preferences.putBoolean(RemoteDebugUIActivator.AUTO_CONNECT, autoConnectButton.getSelection());
		RemoteDebugUIActivator.getDefault().savePreferences();
	}

	@Override
	protected void performDefaults() {
		IEclipsePreferences preferences = RemoteDebugUIActivator.getDefault().getPreferences();
		
//		int keys = RemoteDebugUIActivator.KEYS;
//		for (int i = 0; i < keys; i++) {
//			preferences.put(RemoteDebugUIActivator.getDescriptionPreferenceName(i), RemoteDebugUIActivator.DEFAULT_DESCRIPTION);
//			preferences.put(RemoteDebugUIActivator.getPortPreferenceName(i), RemoteDebugUIActivator.DEFAULT_PORT);
//			preferences.putBoolean(RemoteDebugUIActivator.getShowPreferenceName(i), RemoteDebugUIActivator.DEFAULT_SHOW);
//			
//		}
//		preferences.putBoolean(RemoteDebugUIActivator.DISCOVER_REMOTE_APPLICATION, RemoteDebugUIActivator.DEFAULT_DISCOVER_REMOTE_APPLICATION);
//		RemoteDebugUIActivator.getDefault().savePreferences();
//		remoteDebugs = RemoteDebugUIActivator.getDefault().getRemoteDebugs();
//		viewer.setInput(remoteDebugs);
//		discoverButton.setSelection(RemoteDebugUIActivator.DEFAULT_DISCOVER_REMOTE_APPLICATION);
//		preferences.putBoolean(RemoteDebugUIActivator.DISCOVER_REMOTE_APPLICATION, RemoteDebugUIActivator.DEFAULT_DISCOVER_REMOTE_APPLICATION);
		
		autoConnectButton.setSelection(RemoteDebugUIActivator.AUTO_CONNECT_DEFAULT);
		preferences.putBoolean(RemoteDebugUIActivator.AUTO_CONNECT, RemoteDebugUIActivator.AUTO_CONNECT_DEFAULT);
		RemoteDebugUIActivator.getDefault().savePreferences();
		super.performDefaults();
	}

	@Override
	public boolean performOk() {
		performApply();
		return super.performOk();
	}

}
