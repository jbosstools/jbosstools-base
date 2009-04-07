/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.preferences;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.CheckedTreeSelectionDialog;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.ModelUIMessages;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultXModelObjectLabelProvider;
import org.jboss.tools.common.model.ui.navigator.decorator.DecoratorManager;
import org.jboss.tools.common.model.ui.navigator.decorator.Variable;
import org.jboss.tools.common.model.ui.navigator.decorator.XModelObjectDecorator;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

/**
 * @author Viacheslav Kabanovich
 */
public class DecoratorTextPreferencesPage extends PreferencePage implements IWorkbenchPreferencePage, IPreferencePageExt {
	TreeViewer decorators;
	Composite editor;
	Text formatField;
	ModifyListener formatListener = new ML();
	Button addVariableButton;
	
	TreeViewer examples;
	
	XModelObjectDecorator selection = null;
	
	Map<XModelObjectDecorator,String> currentValues = new HashMap<XModelObjectDecorator, String>();
	
	public DecoratorTextPreferencesPage() {
		setTitle(ModelUIMessages.DecoratorTextPreferencesPage_Text); 
	}

	@Override
	protected Control createContents(Composite parent) {
		init(null);
		Composite g = new Composite(parent, SWT.NONE);
		g.setLayout(new GridLayout(1, false));

		Label label = new Label(g, SWT.NONE);
		label.setText(ModelUIMessages.DecoratorTextPreferencesPage_Decorator); 
		label.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		decorators = new TreeViewer(g, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		decorators.setAutoExpandLevel(2);
		decorators.setLabelProvider(new LabelProvider() {
			public String getText(Object element) {
				if(element instanceof XModelObjectDecorator) {
					XModelObjectDecorator d = (XModelObjectDecorator)element;
					return d.getName();
				}
				return super.getText(element);
			}
			
			public Image getImage(Object element) {
				String entity = null;
				if(element instanceof XModelObjectDecorator) {
					XModelObjectDecorator d = (XModelObjectDecorator)element;
					Set<String> es = d.getEntities();
					entity = (es == null || es.size() == 0) ? null : es.iterator().next();
				} else if(element instanceof String) {
					entity = DecoratorManager.getInstance().getBaseEntityForPartition(element.toString());
				}
				XModelObject o = entity == null ? null : PreferenceModelUtilities.getPreferenceModel().createModelObject(entity, new Properties());
				return (o != null) ? EclipseResourceUtil.getImage(o) : null;
			}
			
		});
		decorators.setContentProvider(new ITreeContentProvider() {

			public Object[] getChildren(Object parentElement) {
				if(parentElement instanceof String) {
					Set<XModelObjectDecorator> set = DecoratorManager.getInstance().getDecoratorsByPartition(parentElement.toString());
					if(set != null) {
						XModelObjectDecorator[] ds = set.toArray(new XModelObjectDecorator[0]);
						Arrays.sort(ds, comparator);
						return ds;
					}
					return new Object[0];
				}
				return new Object[0];
			}

			public Object getParent(Object element) {
				if(element instanceof XModelObjectDecorator) {
					return ((XModelObjectDecorator)element).getPartition();
				}
				return null;
			}

			public boolean hasChildren(Object element) {
				return element instanceof String;
			}

			public Object[] getElements(Object inputElement) {
				if(inputElement == DecoratorManager.getInstance()) {
					return DecoratorManager.getInstance().getPartitions();
				}
				return new Object[0];
			}

			public void dispose() {
			}

			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
				
			}
			
		});
		
		decorators.setInput(DecoratorManager.getInstance());
		
		decorators.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				onSelectionChanged(event.getSelection());
			}
		});
		
		GridData treeLayoutData = new GridData(GridData.FILL_HORIZONTAL);
		treeLayoutData.heightHint = 150;
		decorators.getTree().setLayoutData(treeLayoutData);
		
		Composite c = new Composite(g, SWT.NONE);
		editor = c;
		GridLayout cLayout = new GridLayout(3, false);
		c.setLayout(cLayout);
		c.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		Label cLabel = new Label(c, SWT.NONE);
		cLabel.setText(ModelUIMessages.DecoratorTextPreferencesPage_Format); 
		
		formatField = new Text(c, SWT.SINGLE | SWT.BORDER);
		formatField.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		addVariableButton = new Button(c, SWT.PUSH);
		addVariableButton.setText(ModelUIMessages.DecoratorTextPreferencesPage_AddVariable); 
		
		addVariableButton.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}

			public void widgetSelected(SelectionEvent e) {
				addVariable();
			}
			
		});
		

		label = new Label(g, SWT.NONE);
		label.setText(ModelUIMessages.DecoratorTextPreferencesPage_Preview); 
		label.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		examples = new TreeViewer(g, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		examples.setContentProvider(new ITreeContentProvider() {
			public Object[] getChildren(Object parentElement) {
				return null;
			}
			public Object getParent(Object element) {
				return null;
			}
			public boolean hasChildren(Object element) {
				return false;
			}
			public Object[] getElements(Object inputElement) {
				if(selection != null) {
					return selection.getExamples();
				}
				return new Object[0];
			}
			public void dispose() {
			}
			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
			}
			
		});
		examples.setLabelProvider(new DefaultXModelObjectLabelProvider() {
			public String getText(Object element) {
				if(selection == null) {
					return super.getText(element);
				}
				String result = ""; //$NON-NLS-1$
				if (element instanceof XModelObject) {
					XModelObject modelObject = (XModelObject)element; 
					XModelObjectDecorator d = selection.getWorkingCopy();
					d.setValue(formatField.getText());
					result = d.getLabel(modelObject);
				} else {
					result = ModelUIMessages.DecoratorTextPreferencesPage_DefaultLabel; 
				}
				return result; 
			}
			
		});

		treeLayoutData = new GridData(GridData.FILL_HORIZONTAL);
		treeLayoutData.heightHint = 100;
		examples.getTree().setLayoutData(treeLayoutData);
		
		examples.setInput(DecoratorManager.getInstance());
		
		setSelectedObject(null);
		
		return g;
	}

	public void init(IWorkbench workbench) {
		String[] ps = DecoratorManager.getInstance().getPartitions();
		for (int i = 0; i < ps.length; i++) {
			Set<XModelObjectDecorator> set = DecoratorManager.getInstance().getDecoratorsByPartition(ps[i]);
			for (XModelObjectDecorator d: set) {
				currentValues.put(d, d.getValue());
			}
		}
	}

	public boolean performCancel() {
		return true;
	}

	public boolean performOk() {
		boolean change = false;
		for (XModelObjectDecorator d: currentValues.keySet()) {
			if(equal(d.getValue(), currentValues.get(d))) continue;
			change = true;
			d.setValue(currentValues.get(d));
		}
		if(change) {
			DecoratorManager.getInstance().applyToPreferences();
		}
		return super.performOk();
	}
	
	private boolean equal(String a, String b) {
		if(a == null) return b == null;
		return a.equals(b);
	}
	
	public void performDefaults() {
		for (XModelObjectDecorator d: currentValues.keySet()) {
			String dv = d.getDefaultValue();
			if(dv == null || dv.length() == 0) {
				dv = Variable.NAME.getRuleText();
			}
			currentValues.put(d, dv);
		}
		if(selection != null) {
			setText(currentValues.get(selection));
		}
	}
	
	private void onSelectionChanged(ISelection s) {
		if(!s.isEmpty() && (s instanceof IStructuredSelection)) {
			Object o = ((IStructuredSelection)s).getFirstElement();
			if(o instanceof XModelObjectDecorator) {
				setSelectedObject((XModelObjectDecorator)o);
				return;
			}
		}
		setSelectedObject(null);
	}
	
	private void setSelectedObject(XModelObjectDecorator d) {
		selection = d;
		if(d != null) {
//			editor.setVisible(true);
			formatField.setEnabled(true);
			addVariableButton.setEnabled(true);
			
			String text = currentValues.get(d);
			setText(text);
			
		} else {
//			editor.setVisible(false);
			formatField.setEnabled(false);
			addVariableButton.setEnabled(false);
			setText(""); //$NON-NLS-1$
		}
		examples.refresh(true);
	}
	
	private void setText(String text) {
		if(text == null) text = ""; //$NON-NLS-1$
		formatField.removeModifyListener(formatListener);
		formatField.setText(text);
		formatField.addModifyListener(formatListener);
	}
	
	class ML implements ModifyListener {
		public void modifyText(ModifyEvent e) {
			if(selection != null) {
				currentValues.put(selection, formatField.getText());
				examples.refresh(true);
			}
		}
	}
	
	private void addVariable() {
		CheckedTreeSelectionDialog dialog = new CheckedTreeSelectionDialog(
			addVariableButton.getShell(),
			new LabelProvider(),
			new ITreeContentProvider() {
				public Object[] getChildren(Object parentElement) {
					return null;
				}
				public Object getParent(Object element) {
					return null;
				}
				public boolean hasChildren(Object element) {
					return false;
				}
				public Object[] getElements(Object inputElement) {
					if(selection != null) {
						return selection.getVariables();
					}
					return new Object[0];
				}
				public void dispose() {
				}

				public void inputChanged(Viewer viewer, Object oldInput,
						Object newInput) {
				}
			}			
		);
		dialog.setInput(selection);
		dialog.setTitle(ModelUIMessages.DecoratorTextPreferencesPage_SelectVariable); 
		dialog.create();
		int result = dialog.open();
		if(result == Window.OK) {
			addVariable(dialog.getResult());
		}
	}
	
	private void addVariable(Object[] checked) {
		if(checked == null || checked.length == 0) return;
		String text = formatField.getText();
		int c = formatField.getCaretPosition();
		if(c < 0 || c > text.length()) c = text.length();
		StringBuffer sb = new StringBuffer(text.substring(0, c));
		for (int i = 0; i < checked.length; i++) {
			Variable v = (Variable)checked[i];
			sb.append(v.getRuleText());			
		}
		int cn = sb.length();
		sb.append(text.substring(c));
		text = sb.toString();
		currentValues.put(selection, text);
		setText(text);
		formatField.setSelection(cn);
		examples.refresh(true);
	}
	
	static DComparator comparator = new DComparator();
	static class DComparator implements Comparator<XModelObjectDecorator> {

		public int compare(XModelObjectDecorator o1, XModelObjectDecorator o2) {
			return o1.getName().compareTo(o2.getName());
		}

	}

}
