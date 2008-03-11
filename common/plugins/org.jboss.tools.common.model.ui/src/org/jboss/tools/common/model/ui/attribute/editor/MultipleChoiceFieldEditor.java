/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.attribute.editor;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.*;
import org.jboss.tools.common.model.ui.IListEditor;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.IListContentProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class MultipleChoiceFieldEditor extends ExtendedFieldEditor 
        implements IPropertyFieldEditor, IListEditor, IFieldEditor, PropertyChangeListener {
	protected IPropertyEditor propertyEditor;
	protected IValueProvider valueProvider;
	protected ILabelProvider labelProvider;
	protected IListContentProvider listContentProvider;
	
	protected Composite list;
	protected Button[] boxes = new Button[0];
	
	protected Composite buttonsParent;
	Map<String,Choice> choicesMap = new HashMap<String,Choice>();
	ArrayList<Choice> choicesArray = new ArrayList<Choice>();
	
	public MultipleChoiceFieldEditor() {}
	
	public MultipleChoiceFieldEditor(IWidgetSettings settings) {
		super(settings);
	}
	
	private void addChoice(Choice c) {
		choicesMap.put(c.name, c);
		choicesArray.add(c);
	}
	private void removeChoice(Choice c) {
		choicesMap.remove(c.name);
		choicesArray.remove(c);
	}
	
	class Choice extends SelectionAdapter {
		String name;
		boolean selected;
		boolean allowed = true;
		Button box;
		
		public Choice(String name) {
			this.name = name;
			addChoice(this);
		}
		
		public void setSelected(boolean b) {
			selected = b;
			if(box != null && !box.isDisposed()) box.setSelection(selected);
		}
		
		public void setAllowed(boolean b) {
			allowed = b;
			if(box != null && !box.isDisposed()) {
				int color = (allowed) ? SWT.COLOR_BLACK : SWT.COLOR_DARK_RED;
				box.setForeground(Display.getDefault().getSystemColor(color));
			} 
		}
		
		public void widgetSelected(SelectionEvent e) {
			selected = box.getSelection();
			fireValueChange();
		}
		
		public void create() {
			if(buttonsParent == null || buttonsParent.isDisposed()) return;
			if(box == null || box.isDisposed()) {
				box = new Button(buttonsParent, SWT.CHECK);
				box.setText(name);
				box.addSelectionListener(this);
			}
		}
		
		public void dispose() {
			if(box != null && !box.isDisposed()) {
				box.dispose();
				box = null;
				removeChoice(this);
			}
		}
		
	}
	
	protected Choice getChoice(String name, boolean create) {
		Choice c = (Choice)choicesMap.get(name);
		if(c == null && create) c = new Choice(name);
		if(c != null) c.create();
		return c;
	}

	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			listContentProvider = (IListContentProvider)propertyEditor.getAdapter(IListContentProvider.class);
			labelProvider = (ILabelProvider)propertyEditor.getAdapter(ILabelProvider.class);
		}
		valueProvider.addValueChangeListener(this);
	}

	protected void adjustForNumColumns(int numColumns) {
		Control control = getLabelComposite();
		((GridData)control.getLayoutData()).horizontalSpan = numColumns;
		((GridData)list.getLayoutData()).horizontalSpan = numColumns - 1;
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		Control control = getLabelComposite(parent);
		GridData gd = new GridData();
		gd.horizontalSpan = numColumns;
		control.setLayoutData(gd);
		list = createListControl(parent);
		gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = numColumns - 1;
		gd.grabExcessHorizontalSpace = true;
		list.setLayoutData(gd);
	}

	protected void doLoad() {}

	protected void doLoadDefault() {}

	protected void doStore() {}

	public int getNumberOfControls() {
		return 2;
	}

	public void setLabelProvider(ILabelProvider labelProvider) {
		this.labelProvider = labelProvider;
	}

	public void setListContentProvider(IListContentProvider listContentProvider) {
		this.listContentProvider = listContentProvider;
	}

	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), createListControl(parent)};
	}

	boolean propertyChangeEnabled = true;
	public void propertyChange(PropertyChangeEvent evt) {
		if(!propertyChangeEnabled) return;
		valueProvider.removeValueChangeListener(this);
		if (IPropertyEditor.VALUE.equals(evt.getPropertyName())) {
			updateChoices();
		} else if (IPropertyEditor.LIST_CONTENT.equals(evt.getPropertyName())) {
			DefaultValueAdapter a = (DefaultValueAdapter)propertyEditor.getInput();
			a.load();
			String newValue = (String)valueProvider.getValue();
			valueProvider.setValue("");
			resetChoices();
			valueProvider.setValue(newValue);
		}
		valueProvider.addValueChangeListener(this);
	}

	protected Control getListControl() {
		return list;
	}

	protected Composite createListControl(Composite parent) {
		if (list == null ||list.isDisposed()) {
			ScrolledComposite sp = new ScrolledComposite(parent, SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
			sp.setLayoutData(new GridData(GridData.FILL_BOTH));
			list = sp;
		
			sp.setLayout(new GridLayout());
			Composite c = new Composite(sp, SWT.NONE);
			buttonsParent = c;
			c.setLayoutData(new GridData(GridData.FILL_BOTH));
			sp.setContent(c);
			c.setLayout(new GridLayout());
			resetChoices();
		} else {
			checkParent(list, parent);
		}
		return list;
	}
	
	protected void updateChoices() {
		Set<String> m = new HashSet<String>();
		m.addAll(choicesMap.keySet());
		String[] es = (String[])listContentProvider.getElements(null);
		Set<String> ess = new HashSet<String>();
		for (int i = 0; i < es.length; i++) {
			ess.add(es[i]);
		}
		String value = (String)valueProvider.getValue();
		if(value == null) value = "";
		StringTokenizer st = new StringTokenizer(value, ",;");
		String[] vs = new String[st.countTokens()];
		Set<String> vss = new HashSet<String>();
		for (int i = 0; i < vs.length; i++) {
			vs[i] = st.nextToken();
			vss.add(vs[i]);
		}
		for (int i = 0; i < vs.length; i++) {
			Choice c = getChoice(vs[i], true);
			c.setSelected(true);
			c.setAllowed(ess.contains(vs[i]));
			m.remove(vs[i]);
		}
		for (int i = 0; i < es.length; i++) {
			Choice c = getChoice(es[i], true);
			c.setSelected(vss.contains(es[i]));
			c.setAllowed(true);
			m.remove(es[i]);
		}
		String[] obs = (String[]) m.toArray(new String[0]);
		for (int i = 0; i < obs.length; i++) {
			Choice c = getChoice(obs[i], false);
			if(c != null) {
				c.dispose();
				choicesMap.remove(obs[i]);
			}
		}
		boolean isEmpty = (es.length == 0 && vs.length == 0);
		updateEmpty(isEmpty);
	}
	
	Label empty = null;
	
	void updateEmpty(boolean isEmpty) {
		if(isEmpty == (empty != null && !empty.isDisposed())) return;
		if(isEmpty) {
			empty = new Label(buttonsParent, SWT.NONE);
			empty.setText("Choice list is empty.");
		} else {
			if(empty != null) {
				if(!empty.isDisposed()) empty.dispose();
				empty = null;
			}
		}
	}
	
	protected void resetChoices() {
		updateChoices();
		buttonsParent.pack();
		buttonsParent.layout();
	}

	protected void fireValueChange() {
		StringBuffer sb = new StringBuffer();
		Choice[] cs= (Choice[])choicesArray.toArray(new Choice[0]);
		for (int i = 0; i < cs.length; i++) {
			if(!cs[i].selected) continue;
			if(sb.length() > 0) sb.append(',');
			sb.append(cs[i].name);
		}
		propertyChangeEnabled = false;
		valueProvider.setValue(sb.toString());
		propertyChangeEnabled = true;
	}

	public void setEnabled(boolean enabled){
		// TODO;
		super.setEnabled(enabled);
		if (getListControl()!=null) {
			getListControl().setEnabled(enabled);
		}
	}

	public void setFocus() {
		if(list != null && !list.isDisposed()) {
			list.setFocus();
		}
	}

	public void cut() {}

	public void copy() {}

	public void paste() {}

	public void delete() {}

}
