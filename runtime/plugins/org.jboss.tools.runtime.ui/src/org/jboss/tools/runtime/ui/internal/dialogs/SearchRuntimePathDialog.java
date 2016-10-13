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
package org.jboss.tools.runtime.ui.internal.dialogs;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.ProgressIndicator;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ColumnViewerEditor;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationEvent;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationStrategy;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.TreeViewerEditor;
import org.eclipse.osgi.util.NLS;
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
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.RuntimeInitializerUtil;
import org.jboss.tools.runtime.core.util.RuntimeModelUtil;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.internal.Messages;
import org.jboss.tools.runtime.ui.internal.RuntimeWorkbenchUtils;

/**
 * @author snjeza
 * 
 */
public class SearchRuntimePathDialog extends ProgressMonitorDialog {

	/* 
	 * Moved a method from UIActivator to here bc it clearly
	 * didnt belong there
	 */

	public static SearchRuntimePathDialog launchSearchRuntimePathDialog(Shell shell, 
			final RuntimePath[] runtimePaths, boolean needRefresh, int heightHint) {
		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor) 
					throws InvocationTargetException, InterruptedException {
				RuntimeInitializerUtil.createRuntimeDefinitions(runtimePaths, monitor);
			}
		};
		try {
			HashSet<RuntimePath> set = new HashSet<RuntimePath>(Arrays.asList(runtimePaths));
			
			SearchRuntimePathDialog dialog = new SearchRuntimePathDialog(shell, set, needRefresh, heightHint);
			dialog.run(true, true, op);
			return dialog;
		} catch (InvocationTargetException e1) {
			RuntimeUIActivator.pluginLog().logError(e1);
		} catch (InterruptedException e1) {
			// ignore
		}
		return null;
	}
	
	
	private Set<RuntimePath> runtimePaths = new HashSet<RuntimePath>();
	private boolean running = true;
	private CheckboxTreeViewer treeViewer;
	private boolean canceled;
	private boolean needRefresh;
	private Label foundRuntimesLabel;
	private Button hideCreatedRuntimes;
	private int heightHint;

	public SearchRuntimePathDialog(Shell parent, Set<RuntimePath> runtimePaths, boolean needRefresh, int heightHint) {
		super(parent);
		setShellStyle(SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER
			 | SWT.APPLICATION_MODAL | SWT.RESIZE | getDefaultOrientation());
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
		getShell().setText(Messages.SearchRuntimePathDialog_Searching_for_runtimes);
		
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
		foundRuntimesLabel.setText(""); //$NON-NLS-1$
		
		setMessage(Messages.SearchRuntimePathDialog_Searching, false);
		createMessageArea(composite);
		// Only set for backwards compatibility
		taskLabel = messageLabel;
		
		treeViewer = createRuntimeViewer(runtimePaths, composite, heightHint);
		treeViewer.addCheckStateListener(new ICheckStateListener() {
			
			@Override
			public void checkStateChanged(CheckStateChangedEvent event) {
				RuntimeDefinition definition = (RuntimeDefinition) event.getElement();
				definition.setEnabled(!definition.isEnabled());
				getButton(IDialogConstants.OK_ID).setEnabled(anyDefinitionsChecked());
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
		hideCreatedRuntimes.setText(Messages.SearchRuntimePathDialog_Hide_already_created_runtimes);
		hideCreatedRuntimes.setSelection(true);
		
		hideCreatedRuntimes.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				refresh(null);
			}
		});
		super.createDialogArea(parent);
		return parent;
	}
	
	public static RuntimeCheckboxTreeViewer createRuntimeViewer(final Set<RuntimePath> runtimePaths2, Composite composite, int heightHint) {
		return new RuntimeCheckboxTreeViewer(composite, runtimePaths2, heightHint);
	}

	
	/* are there any definitions enabled / checked? */
	private boolean anyDefinitionsChecked() {
		boolean enableOk = false;
		List<RuntimeDefinition> runtimeDefinitions = getRuntimeDefinitions(true);
		for (RuntimeDefinition runtimeDefinition:runtimeDefinitions) {
			if (runtimeDefinition.isEnabled()) {
				enableOk = true;
			} else {
				for (RuntimeDefinition includedDefinition:runtimeDefinition.getIncludedRuntimeDefinitions()) {
					if (includedDefinition.isEnabled()) {
						enableOk = true;
						break;
					}
				}
			}
			if (enableOk) {
				break;
			}
		}
		return enableOk;
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		// OK button
		Button okButton = createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
				false);
		okButton.setEnabled(false);
		createButton(parent, IDialogConstants.SELECT_ALL_ID, Messages.SearchRuntimePathDialog_Select_All);
		createButton(parent, IDialogConstants.DESELECT_ALL_ID, Messages.SearchRuntimePathDialog_Deselect_All);
		// cancel button
		createCancelButton(parent);
	}
	
	private Button createButton(Composite parent, int id, String label) {
		Button button = createButton(parent, id, label, false);
		button.setEnabled(false);
		return button;
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

	@Override
	protected void buttonPressed(int buttonId) {
		super.buttonPressed(buttonId);
		if (IDialogConstants.SELECT_ALL_ID == buttonId) {
			selectAllPressed();
		} else if (IDialogConstants.DESELECT_ALL_ID == buttonId) {
			deselectAllPressed();
		}
	}

	private void deselectAllPressed() {
		setChecked(false);
		getButton(IDialogConstants.OK_ID).setEnabled(anyDefinitionsChecked());
	}

	private void setChecked(boolean checked) {
		List<RuntimeDefinition> runtimeDefinitions = getRuntimeDefinitions(hideCreatedRuntimes.getSelection());
		for (RuntimeDefinition definition:runtimeDefinitions) {
			treeViewer.setChecked(definition, checked);
			definition.setEnabled(checked);
			for (RuntimeDefinition included:definition.getIncludedRuntimeDefinitions()) {
				treeViewer.setChecked(included, checked);
				included.setEnabled(checked);
			}
		}
		treeViewer.refresh();
	}

	private void selectAllPressed() {
		setChecked(true);
		getButton(IDialogConstants.OK_ID).setEnabled(anyDefinitionsChecked());
	}
	private static boolean runtimeExists(RuntimeDefinition runtimeDefinition) {
		return RuntimeModelUtil.verifyRuntimeDefinitionCreated(runtimeDefinition, false);
	}
	private void refresh(String message) {
		running = false;
		treeViewer.setInput(null);
		List<RuntimeDefinition> runtimeDefinitions = getRuntimeDefinitions(hideCreatedRuntimes.getSelection());
		treeViewer.setInput(runtimeDefinitions);
		for (RuntimeDefinition definition:runtimeDefinitions) {
			treeViewer.setChecked(definition, definition.isEnabled());
			for (RuntimeDefinition included:definition.getIncludedRuntimeDefinitions()) {
				treeViewer.setChecked(included, included.isEnabled());
			}
		}
		TreeItem[] treeItems = treeViewer.getTree().getItems();
		for (TreeItem treeItem:treeItems) {
			Object data = treeItem.getData();
			if (data instanceof RuntimeDefinition) {
				RuntimeDefinition runtimeDefinition = (RuntimeDefinition) data;
				boolean exists = runtimeExists(runtimeDefinition);
				if (exists) {
					treeItem.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_GRAY));
					treeViewer.setChecked(runtimeDefinition, runtimeDefinition.isEnabled());
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
		
		// Get the definitions to be initialized
		final List<RuntimeDefinition> definitions = getEnabledRuntimeDefinitions();
		
		getShell().setCursor(null);
		setReturnCode(OK);
		close();
		
		final boolean needRefresh = this.needRefresh;
		new Job(Messages.SearchRuntimePathDialog_Initializing_runtimes) {
			protected IStatus run(IProgressMonitor monitor) {
				IStatus ret = RuntimeInitializerUtil.initializeRuntimes(definitions);
				if (needRefresh) {
					// Util class runs this in ui thread
					RuntimeWorkbenchUtils.refreshPreferencePage(getShell());
				}
				return ret;
			}
		}.schedule();
	}

	@Override
	protected void finishedRun() {
		if (getShell() == null || getShell().isDisposed()) {
			return;
		}
		decrementNestingDepth();
		getShell().setCursor(null);
		int count = getRuntimeDefinitions(true).size();
		if (count == 0) {
			hideCreatedRuntimes.setSelection(false);
		}
		if (canceled) {
			refresh(Messages.SearchRuntimePathDialog_Searching_runtimes_is_canceled);
		} else {
			refresh(Messages.SearchRuntimePathDialog_Searching_runtimes_is_finished);
			RuntimeModelUtil.updateTimestamps(runtimePaths.toArray(new RuntimePath[runtimePaths.size()]));
		}
		
		String foundRuntimes;
		if (count == 0) {
			foundRuntimes = Messages.SearchRuntimePathDialog_No_runtime_found;
			getButton(IDialogConstants.OK_ID).setEnabled(false);
		} else {
			if (count == 1) {
				foundRuntimes = NLS.bind(Messages.SearchRuntimePathDialog_New_runtime_found, count);
			} else {
				foundRuntimes = NLS.bind(Messages.SearchRuntimePathDialog_New_runtimes_found, count);
			}
			getButton(IDialogConstants.OK_ID).setEnabled(true);
			getButton(IDialogConstants.SELECT_ALL_ID).setEnabled(true);
			getButton(IDialogConstants.DESELECT_ALL_ID).setEnabled(true);
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

	private List<RuntimeDefinition> getEnabledRuntimeDefinitions() {
		List<RuntimeDefinition> all = getRuntimeDefinitions(true);
		Iterator<RuntimeDefinition> i = all.iterator();
		while(i.hasNext()) {
			RuntimeDefinition rd = i.next();
			if( !rd.isEnabled()) {
				boolean add = false;
				for (RuntimeDefinition ird:rd.getIncludedRuntimeDefinitions() ) {
					if (ird.isEnabled()) {
						add = true;
						break;
					}
				}
				if (!add) {
					i.remove();
				}
			}
		}
		return all;
	}
	
	private List<RuntimeDefinition> getRuntimeDefinitions(
			boolean hideCreatedRuntimes) {
		List<RuntimeDefinition> runtimeDefinitions = new ArrayList<RuntimeDefinition>();
		
		List<RuntimeDefinition> allDefinitions = RuntimeModelUtil.getAllDefinitions(runtimePaths.toArray(new RuntimePath[0]));
		for (RuntimePath runtimePath : runtimePaths) {
			List<RuntimeDefinition> pathDefinitions = RuntimeModelUtil.getAllDefinitions(runtimePath);
			for (RuntimeDefinition runtimeDefinition : pathDefinitions) {
				if (!RuntimeModelUtil.verifyRuntimeDefinitionCreated(runtimeDefinition)) {
					String name = runtimeDefinition.getName();
					int i = 2;
					while (runtimeDefinitionExists(runtimeDefinition, allDefinitions)) {
						runtimeDefinition.setName(name + " (" + i++ + ")"); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			}
		}
		
		for (RuntimePath runtimePath : runtimePaths) {
			for (RuntimeDefinition runtimeDefinition : runtimePath.getRuntimeDefinitions()) {
				if (runtimeDefinitions.contains(runtimeDefinition)) {
					continue;
				}
				if (!hideCreatedRuntimes) {
					runtimeDefinitions.add(runtimeDefinition);
				} else if (!RuntimeModelUtil.verifyRuntimeDefinitionCreated(runtimeDefinition)) {
					runtimeDefinitions.add(runtimeDefinition);
				}
			}
		}
		Collections.sort(runtimeDefinitions, new Comparator<RuntimeDefinition>() {

			@Override
			public int compare(RuntimeDefinition o1, RuntimeDefinition o2) {
				if (o1 == null && o2 == null) {
					return 0;
				}
				if (o1 == null || o1.getName() == null) {
					return -1;
				}
				if (o2 == null) {
					return 1;
				}
				return o1.getName().compareTo(o2.getName());
			}
		});
		return runtimeDefinitions;
	}

	private boolean runtimeDefinitionExists(RuntimeDefinition runtimeDefinition,
			List<RuntimeDefinition> allRuntimeDefinitions) {
		String name = runtimeDefinition.getName();
		File location = runtimeDefinition.getLocation();
		String type = runtimeDefinition.getType();
		if (name == null || location == null || type == null) {
			return false;
		}
		String path = location.getAbsolutePath();
		if (path == null) {
			return false;
		}
		for (RuntimeDefinition definition:allRuntimeDefinitions) {
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
