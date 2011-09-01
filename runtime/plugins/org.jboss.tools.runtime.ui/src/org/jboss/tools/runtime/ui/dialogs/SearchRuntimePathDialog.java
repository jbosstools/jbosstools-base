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

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.ProgressIndicator;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ColumnViewerEditor;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationEvent;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationStrategy;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.TreeViewerEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TreeItem;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.model.ServerDefinition;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * @author snjeza
 * 
 */
public class SearchRuntimePathDialog extends ProgressMonitorDialog {

	private Set<RuntimePath> runtimePaths = new HashSet<RuntimePath>();
	private boolean running = true;
	private CheckboxTreeViewer treeViewer;
	private boolean canceled;
	private boolean needRefresh;
	private Label foundRuntimesLabel;
	private List<ServerDefinition> serverDefinitions;
	private Button hideCreatedRuntimes;
	private int heightHint;

	public SearchRuntimePathDialog(Shell parent, Set<RuntimePath> runtimePaths, boolean needRefresh, int heightHint) {
		super(parent);
		setShellStyle(SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER
			 | SWT.RESIZE | getDefaultOrientation());
		this.runtimePaths = runtimePaths;
		this.needRefresh = needRefresh;
		this.heightHint = heightHint;
	}
	
	/**
	 * Set the message in the message label.
	 * 
	 * @param messageString
	 *            The string for the new message.
	 * @param force
	 *            If force is true then always set the message text.
	 */
	private void setMessage(String messageString, boolean force) {
		// must not set null text in a label
		message = messageString == null ? "" : messageString; //$NON-NLS-1$
		if (messageLabel == null || messageLabel.isDisposed()) {
			return;
		}
		if (force || messageLabel.isVisible()) {
			messageLabel.setToolTipText(message);
			messageLabel.setText(shortenText(message, messageLabel));
		}
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		getShell().setText("Searching for runtimes...");
		
		Composite composite = new Composite(parent, SWT.NONE); 
		GridData  gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.horizontalSpan = 2;
		//gd.heightHint = convertVerticalDLUsToPixels(15);
		gd.minimumHeight = convertVerticalDLUsToPixels(heightHint);
		composite.setLayoutData(gd);
		composite.setLayout(new GridLayout(1, false));
		
		foundRuntimesLabel = new Label(composite, SWT.NONE);
		gd = new GridData(GridData.FILL, SWT.FILL, true, false);
		gd.horizontalSpan = 2;
		foundRuntimesLabel.setLayoutData(gd);
		foundRuntimesLabel.setFont(parent.getFont());
		foundRuntimesLabel.setText("");
		
		setMessage("Searching...", false);
		createMessageArea(composite);
		// Only set for backwards compatibility
		taskLabel = messageLabel;
		
		treeViewer = RuntimeUIActivator.createRuntimeViewer(runtimePaths, composite, heightHint);
		treeViewer.addCheckStateListener(new ICheckStateListener() {
			
			@Override
			public void checkStateChanged(CheckStateChangedEvent event) {
				ServerDefinition definition = (ServerDefinition) event.getElement();
				definition.setEnabled(!definition.isEnabled());
				boolean enableOk = false;
				List<ServerDefinition> serverDefinitions = getServerDefinitions(hideCreatedRuntimes.getSelection());
				for (ServerDefinition serverDefinition:serverDefinitions) {
					if (serverDefinition.isEnabled()) {
						enableOk = true;
					}
				}
				getButton(IDialogConstants.OK_ID).setEnabled(enableOk);
			}
		});
		
		ColumnViewerEditorActivationStrategy actSupport = new ColumnViewerEditorActivationStrategy(treeViewer) {
			protected boolean isEditorActivationEvent(
					ColumnViewerEditorActivationEvent event) {
				return event.eventType == ColumnViewerEditorActivationEvent.TRAVERSAL
						|| event.eventType == ColumnViewerEditorActivationEvent.MOUSE_DOUBLE_CLICK_SELECTION
						|| event.eventType == ColumnViewerEditorActivationEvent.PROGRAMMATIC;
			}
		};

		TreeViewerEditor.create(treeViewer, actSupport,
				ColumnViewerEditor.TABBING_HORIZONTAL
						| ColumnViewerEditor.TABBING_MOVE_TO_ROW_NEIGHBOR
						| ColumnViewerEditor.TABBING_VERTICAL
						| ColumnViewerEditor.KEYBOARD_ACTIVATION);

		// label showing current task
		subTaskLabel = new Label(parent, SWT.LEFT | SWT.WRAP);
		gd = new GridData(GridData.FILL, SWT.FILL, true, false);
		gd.horizontalSpan = 2;
		gd.heightHint = 0;
		subTaskLabel.setLayoutData(gd);
		subTaskLabel.setFont(parent.getFont());
		// progress indicator
		progressIndicator = new ProgressIndicator(parent);
		gd = new GridData(GridData.FILL, SWT.FILL, true, false);
		gd.horizontalSpan = 2;
		gd.heightHint = 0;
		progressIndicator.setLayoutData(gd);
		
		hideCreatedRuntimes = new Button(parent, SWT.CHECK);
		gd = new GridData(GridData.FILL, SWT.FILL, true, false);
		gd.horizontalSpan = 2;
		hideCreatedRuntimes.setLayoutData(gd);
		hideCreatedRuntimes.setText("Hide already created runtimes");
		hideCreatedRuntimes.setSelection(true);
		
		hideCreatedRuntimes.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				refresh(null);
			}
		
		});
		
		return parent;
		
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		// OK button
		Button okButton = createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
				true);
		okButton.setEnabled(false);
		// cancel button
		createCancelButton(parent);
	}

	@Override
	protected void createCancelButton(Composite parent) {
		cancel = createButton(parent, IDialogConstants.CANCEL_ID,
				IDialogConstants.CANCEL_LABEL, false);
		if (arrowCursor == null) {
			arrowCursor = new Cursor(cancel.getDisplay(), SWT.CURSOR_ARROW);
		}
		cancel.setCursor(arrowCursor);
		setOperationCancelButtonEnabled(enableCancelButton);
	}
	
	@Override
	protected void cancelPressed() {
		getProgressMonitor().setCanceled(true);
		if (running) {
			canceled = true;
		} else {
			setReturnCode(CANCEL);
			close();
		}
	}

	private void refresh(String message) {
		running = false;
		treeViewer.setInput(null);
		List<ServerDefinition> serverDefinitions = getServerDefinitions(hideCreatedRuntimes.getSelection());
		treeViewer.setInput(serverDefinitions);
		for (ServerDefinition definition:serverDefinitions) {
			treeViewer.setChecked(definition, definition.isEnabled());
			for (ServerDefinition included:definition.getIncludedServerDefinitions()) {
				treeViewer.setChecked(included, included.isEnabled());
			}
		}
		TreeItem[] treeItems = treeViewer.getTree().getItems();
		for (TreeItem treeItem:treeItems) {
			Object data = treeItem.getData();
			if (data instanceof ServerDefinition) {
				ServerDefinition serverDefinition = (ServerDefinition) data;
				boolean exists = RuntimeUIActivator.runtimeExists(serverDefinition);
				if (exists) {
					treeItem.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_GRAY));
					treeViewer.setChecked(serverDefinition, serverDefinition.isEnabled());
				}
			}
		}
		if (message != null) {
			setMessage(message, false);
		}
	}

	@Override
	protected void okPressed() {
		getShell().setCursor(Display.getCurrent().getSystemCursor(SWT.CURSOR_WAIT));
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getRuntimeDetectors();
		List<ServerDefinition> definitions = getServerDefinitions(true);
		for( IRuntimeDetector detector:detectors) {
			if (detector.isEnabled()) {
				detector.initializeRuntimes(definitions);
			}
		}
		getShell().setCursor(null);
		setReturnCode(OK);
		close();
		if (needRefresh) {
			Display.getCurrent().asyncExec(new Runnable() {
				
				public void run() {
					RuntimeUIActivator.refreshPreferencePage(getShell());
				}
			});
			
		}
	}

	@Override
	protected void finishedRun() {
		decrementNestingDepth();
		getShell().setCursor(null);
		int count = getServerDefinitions(true).size();
		if (count == 0) {
			hideCreatedRuntimes.setSelection(false);
		}
		if (canceled) {
			refresh("Searching runtimes is canceled.");
		} else {
			refresh("Searching runtimes is finished.");
			RuntimeUIActivator.setTimestamp(runtimePaths);
		}
		
		String foundRuntimes;
		if (count == 0) {
			foundRuntimes = "No runtime found.";
			getButton(IDialogConstants.OK_ID).setEnabled(false);
		} else {
			if (count == 1) {
				foundRuntimes = count + " new runtime found. Press OK to create the runtimes with a checkmark.";
			} else {
				foundRuntimes = count + " new runtimes found. Press OK to create the runtimes with a checkmark.";
			}
			getButton(IDialogConstants.OK_ID).setEnabled(true);
		}
		foundRuntimesLabel.setText(foundRuntimes);
	}
	
	@Override
	protected Image getImage() {
		return null;
	}
	
	@Override
	protected Control createMessageArea(Composite composite) {
		// create message
		if (message != null) {
			messageLabel = new Label(composite, getMessageLabelStyle());
			messageLabel.setText(message);
			GridDataFactory
					.fillDefaults()
					.align(SWT.FILL, SWT.BEGINNING)
					.grab(true, false)
					.applyTo(messageLabel);
		}
		return composite;
	}

	private List<ServerDefinition> getServerDefinitions(
			boolean hideCreatedRuntimes) {
		if (serverDefinitions == null) {
			serverDefinitions = new ArrayList<ServerDefinition>();
		} else {
			serverDefinitions.clear();
		}
		for (RuntimePath runtimePath : runtimePaths) {
			for (ServerDefinition serverDefinition : runtimePath
					.getServerDefinitions()) {
				if (!RuntimeUIActivator.runtimeCreated(serverDefinition)) {
					List<ServerDefinition> allServerDefinitions = RuntimeUIActivator.getDefault().getServerDefinitions();
					String name = serverDefinition.getName();
					int i = 2;
					while (serverDefinitionsExists(serverDefinition, allServerDefinitions)) {
						serverDefinition.setName(name + " (" + i++ + ")");
					}
				}
				if (!hideCreatedRuntimes) {
					serverDefinitions.add(serverDefinition);
				} else if (!RuntimeUIActivator.runtimeCreated(serverDefinition)) {
					serverDefinitions.add(serverDefinition);
				}
			}

		}
		return serverDefinitions;
	}

	private boolean serverDefinitionsExists(ServerDefinition serverDefinition,
			List<ServerDefinition> allServerDefinitions) {
		String name = serverDefinition.getName();
		File location = serverDefinition.getLocation();
		String type = serverDefinition.getType();
		if (name == null || location == null || type == null) {
			return false;
		}
		String path = location.getAbsolutePath();
		if (path == null) {
			return false;
		}
		for (ServerDefinition definition:allServerDefinitions) {
			if (name.equals(definition.getName()) && type.equals(definition.getType())) {
				File loc = definition.getLocation();
				if (loc == null) {
					continue;
				}
				String dPath = loc.getAbsolutePath();
				if (dPath == null) {
					continue;
				}
				if (!path.equals(dPath)) {
					return true;
				}
			}
		}
		return false;
	}

}
