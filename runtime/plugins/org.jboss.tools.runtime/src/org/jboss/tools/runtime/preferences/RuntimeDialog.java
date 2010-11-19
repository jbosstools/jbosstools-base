package org.jboss.tools.runtime.preferences;

import java.io.File;
import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.jboss.tools.runtime.ServerDefinition;

public class RuntimeDialog extends Dialog {
	
	private List<ServerDefinition> serverDefinitions;
	private String path;
	private CheckboxTableViewer tableViewer;

	public RuntimeDialog(Shell parentShell, List<ServerDefinition> serverDefinitions , String path) {
		super(parentShell);
		setShellStyle(SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER
				| SWT.MODELESS | SWT.RESIZE | getDefaultOrientation());
		this.serverDefinitions = serverDefinitions;
		this.path = path;
		
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite area = (Composite) super.createDialogArea(parent);
		Composite contents = new Composite(area, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = 400;
		gd.widthHint = 700;
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout());
		getShell().setText("JBoss Runtimes");
		applyDialogFont(contents);
		initializeDialogUnits(area);

		Label label = new Label(contents, SWT.NULL);
		String runtime = (serverDefinitions.size() == 1) ? "runtime" : "runtimes";
		label.setText("The search found " + serverDefinitions.size() + " " + runtime + " while searching " + path + ".");
		tableViewer = CheckboxTableViewer.newCheckList(contents, SWT.V_SCROLL
				| SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE);
		Table table = tableViewer.getTable();
		gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = 100;
		table.setLayoutData(gd);
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		String[] columnNames = new String[] { "Name", "Version", "Type", "Location", "Description"};
		int[] columnWidths = new int[] { 140, 50, 50, 245, 200};
		
		for (int i = 0; i < columnNames.length; i++) {
			TableColumn tc = new TableColumn(table, SWT.LEFT);
			tc.setText(columnNames[i]);
			tc.setWidth(columnWidths[i]);
		}

		tableViewer.setLabelProvider(new RuntimeLabelProvider());
		tableViewer.setContentProvider(new RuntimeContentProvider(serverDefinitions));
		tableViewer.setInput(serverDefinitions);
		for (ServerDefinition definition:serverDefinitions) {
			tableViewer.setChecked(definition, definition.isEnabled());
		}
		tableViewer.addCheckStateListener(new ICheckStateListener() {
			
			public void checkStateChanged(CheckStateChangedEvent event) {
				ServerDefinition definition = (ServerDefinition) event.getElement();
				definition.setEnabled(!definition.isEnabled());
				boolean enableOk = false;
				for (ServerDefinition serverDefinition:serverDefinitions) {
					if (serverDefinition.isEnabled()) {
						enableOk = true;
					}
				}
				getButton(IDialogConstants.OK_ID).setEnabled(enableOk);
			}
		});
		return area;
	}
	
	private class RuntimeLabelProvider extends LabelProvider implements
			ITableLabelProvider {

		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

		public String getColumnText(Object element, int columnIndex) {
			if (element instanceof ServerDefinition) {
				ServerDefinition definition = (ServerDefinition) element;
				if (columnIndex == 0) {
					return definition.getName();
				}
				if (columnIndex == 1) {
					return definition.getVersion();
				}
				if (columnIndex == 2) {
					return definition.getType();
				}
				if (columnIndex == 3) {
					File location = definition.getLocation();
					if (location != null) {
						return definition.getLocation().getAbsolutePath();
					}
				}
				if (columnIndex == 4) {
					return definition.getDescription();
				}
			}
			return null;
		}
	}
	
	private class RuntimeContentProvider implements IStructuredContentProvider {

		private List<ServerDefinition> serverDefinitions;

		public RuntimeContentProvider(List<ServerDefinition> serverDefinitions) {
			this.serverDefinitions = serverDefinitions;
		}
		
		public Object[] getElements(Object inputElement) {
			return serverDefinitions.toArray();
		}

		public void dispose() {
			
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			serverDefinitions = (List<ServerDefinition>) newInput;
		}
		
	}

}
