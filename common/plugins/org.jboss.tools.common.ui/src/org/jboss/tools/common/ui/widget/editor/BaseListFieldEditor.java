/******************************************************************************* 
 * Copyright (c) 2010 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/

package org.jboss.tools.common.ui.widget.editor;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.ui.CommonUIMessages;

/**
 * This editor allows to add to a list several items by 'Add' button.
 * If ListFieldEditorProvider is not set, selection dialog will have nothing to pick up. 
 * 
 * @author Viacheslav Kabanovich
 */
public class BaseListFieldEditor extends BaseFieldEditor {	
	private ListViewer viewer = null;
	private ActionPanel actionPanel;

	public BaseListFieldEditor(String name, String label, Object defaultValue) {
		super(name, label, defaultValue);
	}

	@Override
	public Object[] getEditorControls(Object composite) {
		Composite root = (Composite)composite;
		createLabelControl(root);
		createListView(root);
		createActionBar(root);
		return  new Control[] {getLabelControl(), viewer.getControl(), actionPanel};
	}

	@SuppressWarnings("unchecked")
	protected void createListView(Composite parent) {
		viewer = new ListViewer(parent, SWT.V_SCROLL | SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI);
		viewer.setContentProvider(new IStructuredContentProvider() {

			public Object[] getElements(Object inputElement) {
				return (inputElement instanceof List) ?	((List<Object>) inputElement).toArray() : new Object[0];
			}

			public void dispose() {
			}

			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
				viewer.refresh();
			}
		});
		viewer.setLabelProvider(createLabelProvider());
		viewer.setInput(getValue());
	}

	protected ILabelProvider createLabelProvider() {
		return new LabelProvider();
	}

	protected void createActionBar(Composite parent) {
		actionPanel = new ActionPanel(parent, new BaseAction[] { new AddAction(),
				new RemoveAction() });
		viewer.addSelectionChangedListener(actionPanel);
	}

	@Override
	public Object[] getEditorControls() {
		return new Control[]{getLabelControl(), viewer.getControl(), actionPanel};//new Control[] { root };
	}

	@Override
	public int getNumberOfControls() {
		return 3;
	}

	@Override
	public void doFillIntoGrid(Object parent) {
		Assert.isTrue(parent instanceof Composite,
				"Error_Basic_Editor_Composite");
		Assert.isTrue(((Composite) parent).getLayout() instanceof GridLayout,
				"Error_JBoss_Basic_Editor_Support");
		Composite aComposite = (Composite) parent;
		getEditorControls(aComposite);
		GridLayout gl = (GridLayout) ((Composite) parent).getLayout();

		GridData gdLabel = new GridData();
		gdLabel.horizontalSpan = 1;
		getLabelControl().setLayoutData(gdLabel);

		GridData gd = new GridData();
		gd.horizontalSpan = gl.numColumns - 2;
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = GridData.FILL;
		gd.verticalAlignment = GridData.FILL;
		((Control) getEditorControls()[1]).setLayoutData(gd);
		
		GridData dgAction = new GridData();
		dgAction.horizontalSpan = 1;
		dgAction.verticalAlignment = GridData.FILL;
		((Control) getEditorControls()[2]).setLayoutData(dgAction);
	}

	protected int convertHorizontalDLUsToPixels(Control control, int dlus) {
		GC gc= new GC(control);
		gc.setFont(control.getFont());
		int averageWidth= gc.getFontMetrics().getAverageCharWidth();
		gc.dispose();
		double horizontalDialogUnitSize = averageWidth * 0.25;
		return (int)Math.round(dlus * horizontalDialogUnitSize);
	}
	
	public class ActionPanel extends Composite implements ISelectionChangedListener {
		private BaseAction[] actions = null;

		public ActionPanel(Composite parent, int style, BaseAction[] actions) {
			super(parent, style);
			this.actions = actions;
			setLayout(new GridLayout(1, false));
			for (BaseAction action : this.actions) {
				new ActionButton(this, SWT.PUSH, action);
			}
		}

		public ActionPanel(Composite parent, BaseAction[] actions) {
			this(parent, SWT.NONE, actions);
		}

		public void selectionChanged(SelectionChangedEvent event) {
			for (BaseAction action : actions) {
				action.setSelection(event.getSelection());
			}
		}
	}

	public class ActionButton implements IPropertyChangeListener {
		private Button button;
		private BaseAction action;

		public ActionButton(Composite parent, int style, BaseAction action) {
			this.button = new Button(parent, style);
			this.action = action;

			GridData gd = new GridData(GridData.FILL_HORIZONTAL, GridData.BEGINNING, false, false);

			gd.horizontalAlignment = GridData.FILL;
			gd.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
			gd.widthHint = convertHorizontalDLUsToPixels(button, IDialogConstants.BUTTON_WIDTH);
			this.button.setLayoutData(gd);
			this.action.addPropertyChangeListener(this);
			this.button.setText(action.getText());
			this.button.setEnabled(action.isEnabled());
			this.button.addSelectionListener(new SelectionListener() {
				public void widgetSelected(SelectionEvent e) {
					ActionButton.this.action.run();
				}

				public void widgetDefaultSelected(SelectionEvent e) {
				}
			});

		}

		public Control getControl() {
			return button;
		}

		public void propertyChange(PropertyChangeEvent event) {
			if (event.getProperty().equals(IAction.ENABLED)) {
				button.setEnabled(((Boolean) event.getNewValue())
						.booleanValue());
			}
		}
	}

	public static abstract class BaseAction extends Action {
		Object[] objects = new Object[0];

		public BaseAction(String name) {
			super(name);
			updateEnablement();
		}

		public void setSelection(ISelection selection) {
			if (selection instanceof IStructuredSelection) {
				List<Object> rts = new ArrayList<Object>();
				for (Object rt : ((IStructuredSelection) selection).toArray()) {
					rts.add(rt);
				}
				objects = rts.toArray(new Object[] {});
			} else {
				objects = new Object[0];
			}
			updateEnablement();
		}

		protected abstract void updateEnablement();
	}

	public class AddAction extends BaseAction {

		public AddAction() {
			super(CommonUIMessages.BUTTON_ADD);
			setEnabled(true);
		}

		@Override
		protected void updateEnablement() {
		}

		@Override
		public void run() {
			List<?> current = (List<?>)getValue();
			List<Object> added = runAddAction();
			added.removeAll(current);
			if(!added.isEmpty()) {
				added.addAll(current);
				setValue(added);
				viewer.setInput(getValue());
				viewer.refresh();
			}
		}
	}

	/**
	 * Returns list of objects to be added.
	 * @return
	 */
	protected List<Object> runAddAction() {
		return new ArrayList<Object>();
	}

	public class RemoveAction extends BaseAction {
		public RemoveAction() {
			super(CommonUIMessages.BUTTON_REMOVE);
		}

		@Override
		protected void updateEnablement() {
			setEnabled(objects.length > 0);
		}

		@Override
		public void run() {
			List<?> current = (List<?>)getValue();
			List<Object> changed = new ArrayList<Object>();
			changed.addAll(current);
			for (Object rt : objects) {
				changed.remove(rt);
			}
			if(changed.size() != current.size()) {
				setValue(changed);
			}
			viewer.setInput(getValue());
			viewer.refresh();
		}

	}

}
