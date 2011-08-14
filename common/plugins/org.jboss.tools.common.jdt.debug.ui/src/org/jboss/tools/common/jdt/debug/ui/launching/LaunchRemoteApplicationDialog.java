package org.jboss.tools.common.jdt.debug.ui.launching;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnLayoutData;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.VmModel;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;
import org.jboss.tools.common.jdt.debug.ui.actions.LaunchDialogAction;
import org.jboss.tools.common.jdt.debug.ui.actions.RemoteLaunchAction;
import org.jboss.tools.common.jdt.debug.ui.preferences.AutoResizeTableLayout;

/**
 * @author snjeza
 * 
 */
public class LaunchRemoteApplicationDialog extends Dialog {
	
	private VmModel[] vmModels;
	private TableViewer viewer;
	private Combo configurationsCombo;

	public LaunchRemoteApplicationDialog(Shell parentShell) {
		super(parentShell);
		setShellStyle(SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER
				| SWT.RESIZE | getDefaultOrientation());
		vmModels = RemoteDebugUIActivator.getDefault().getCurrentDebugModels();
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		getShell().setText("Remote Java Application");
		Composite area = (Composite) super.createDialogArea(parent);
		Composite contents = new Composite(area, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = 400;
		gd.widthHint = 600;
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
		applyDialogFont(contents);
		initializeDialogUnits(area);

		Composite messageComposite = new Composite(contents, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		messageComposite.setLayoutData(gd);
		messageComposite.setLayout(new GridLayout(1, false));
		Label pathLabel = new Label(messageComposite, SWT.NONE);
		pathLabel.setText("Found " + vmModels.length + " Remote Java Application's. Please select which application you want to connect the debugger to.");
		
		viewer = new TableViewer(contents, SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.BORDER);
		gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = 250;
		viewer.getTable().setLayoutData(gd);
		
		final Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		table.setFont(parent.getFont());
		table.addSelectionListener(new SelectionAdapter() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				int selection = table.getSelectionIndex();
				Button okButton = getButton(IDialogConstants.OK_ID);
				if (selection < 0 || selection >= vmModels.length) {
					okButton.setEnabled(false);
				} else {
					okButton.setEnabled(true);
				}
			}
			
		});
		
		viewer.setContentProvider(new RemoteApplicationContentProvider());
		
		String[] columnHeaders = {"PID", "Port", "Class", "Arguments"};
		
		for (int i = 0; i < columnHeaders.length; i++) {
			TableViewerColumn column = new TableViewerColumn(viewer, SWT.NONE);
			column.setLabelProvider(new RemoteApplicationLabelProvider(i));
			column.getColumn().setText(columnHeaders[i]);
			column.getColumn().setResizable(true);
			column.getColumn().setMoveable(true);
		}
		
		ColumnLayoutData[] remoteApplicationLayouts= {
				new ColumnWeightData(80,80),
				new ColumnWeightData(80,80),
				new ColumnWeightData(200,200),
				new ColumnWeightData(200,200),
			};
		
		TableLayout tableLayout = new AutoResizeTableLayout(table);
		for (int i = 0; i < remoteApplicationLayouts.length; i++) {
			tableLayout.addColumnData(remoteApplicationLayouts[i]);
		}
		
		viewer.getTable().setLayout(tableLayout);
		
		viewer.setInput(vmModels);
		
		viewer.getTable().select(0);
	
		Composite configurationComposite = new Composite(contents, SWT.NONE);
		configurationComposite.setLayout(new GridLayout(2, false));
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		configurationComposite.setLayoutData(gd);
		
		Label configurationLabel = new Label(configurationComposite, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		configurationLabel.setLayoutData(gd);
		configurationLabel.setText("Configurations:");
		configurationsCombo = new Combo(configurationComposite, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		configurationsCombo.setLayoutData(gd);
		configureCombo();
			
		final Button autoButton = new Button(contents, SWT.CHECK);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		messageComposite.setLayoutData(gd);
		autoButton.setText("Automatically connect if only one application found");
		autoButton.setSelection(RemoteDebugUIActivator.getDefault().isAutoConnect());
		autoButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				RemoteDebugUIActivator.getDefault().setAutoConnect(autoButton.getSelection());
			}
			
		});
		
		return area;
	}

	private void configureCombo() {
		String[] configurations = getConfigurations();
		configurationsCombo.setItems(configurations);
		configurationsCombo.select(getDefaultConfigurationIndex(configurations));
	}

	private int getDefaultConfigurationIndex(String[] configurations) {
		if (configurations.length == 0) {
			return 0;
		}
		ILaunchConfiguration defaultConfiguration = RemoteDebugActivator.getDefault().getDefaultLaunchConfiguration();
		if (defaultConfiguration == null) {
			return 0;
		}
		for (int i = 0; i < configurations.length; i++) {
			String name = configurations[i];
			if (name.equals(defaultConfiguration.getName())) {
				return i;
			}
		}
		return 0;
	}

	private String[] getConfigurations() {
		ILaunchConfiguration[] configs = RemoteDebugActivator.getDefault().getLaunchConfigurations();
		if (configs == null || configs.length == 0) {
			try {
				ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
				ILaunchConfigurationType type = manager.getLaunchConfigurationType(RemoteDebugActivator.REMOTE_JAVA_APPLICATION_ID);
				ILaunchConfigurationWorkingCopy config = RemoteDebugActivator.createNewLaunchConfiguration(type);
				return new String[] { config.getName() };
			} catch (CoreException e) {
				RemoteDebugUIActivator.log(e);
				return new String[0];
			}
		}
		String[] configNames = new String[configs.length];
		for (int i = 0; i < configs.length; i++) {
			configNames[i] = configs[i].getName();
 		}
		return configNames;
	}

	class RemoteApplicationContentProvider implements IStructuredContentProvider {

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			
		}

		@Override
		public Object[] getElements(Object inputElement) {
			return vmModels;
		}
		
		@Override
		public void dispose() {
			
		}

	}
	
	class RemoteApplicationLabelProvider extends ColumnLabelProvider {

		private int columnIndex;

		public RemoteApplicationLabelProvider(int i) {
			this.columnIndex = i;
		}

		public String getText(Object element) {
			if (element instanceof VmModel) {
				VmModel model = (VmModel) element;
				switch (columnIndex) {
				case 0:
					return model.getPid();
				case 1:
					return model.getPort();
				case 2:
					return model.getMainClass();
				case 3:
					return model.getMainArgs();
				}
			}
			return null;
		}

		@Override
		public Image getImage(Object element) {
			return null;
		}
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.YES_ID, "Configure...", false);
		super.createButtonsForButtonBar(parent);
		Button okButton = getButton(IDialogConstants.OK_ID);
		okButton.setText("Debug");
	}

	@Override
	protected void okPressed() {
		VmModel vmModel = vmModels[viewer.getTable().getSelectionIndex()];
		String configurationName = configurationsCombo.getText();
		ILaunchConfiguration[] configurations = RemoteDebugActivator.getDefault().getLaunchConfigurations();
		ILaunchConfiguration selectedConfiguration = null;
		for (ILaunchConfiguration configuration:configurations) {
			if (configurationName.equals(configuration.getName())) {
				selectedConfiguration = configuration;
			} else {
				setDefault(configuration, false);
			}
		}
		if (selectedConfiguration != null) {
			setDefault(selectedConfiguration, true);
		}
		super.okPressed();
		new RemoteLaunchAction(vmModel.getPort()).run();
	}

	private void setDefault(ILaunchConfiguration configuration, boolean value) {
		try {
			boolean isDefault = configuration.getAttribute(RemoteDebugActivator.SET_AS_DEFAULT, false);
			if (isDefault != value) {
				ILaunchConfigurationWorkingCopy wc = configuration.getWorkingCopy();
				wc.setAttribute(RemoteDebugActivator.SET_AS_DEFAULT, value);
				wc.doSave();
			}
		} catch (CoreException e) {
			RemoteDebugUIActivator.log(e);
		}
	}

	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.YES_ID) {
			new LaunchDialogAction().run();
			configureCombo();
		} else {
			super.buttonPressed(buttonId);
		}
	}
	
}
