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
package org.jboss.tools.common.model.ui.objecteditor;

import java.util.Properties;
import java.util.Set;

import org.jboss.tools.common.editor.AbstractSelectionProvider;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.action.CommandBarListener;
import org.jboss.tools.common.model.ui.dnd.*;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.jboss.tools.common.model.ui.swt.util.BorderLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Widget;

import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.AbstractTableHelper;

public class XChildrenEditor implements CommandBarListener {
	protected static Color DEFAULT_COLOR = new Color(null, 0, 0, 0);
	protected static Color GREYED_COLOR = new Color(null, 127, 127, 127);
	public static String ADD = "&Add";
	public static String DELETE = "&Delete";
	public static String EDIT = "&Edit";
	public static String UP = "&Up";
	public static String DOWN = "Do&wn";
	protected Composite control; 
	protected AbstractTableHelper helper = createHelper();
	protected XTable xtable = new XTable();
	protected boolean lock = false;
	protected CommandBar bar = new CommandBar();
	protected SelectionListener selectionListener = null;
	DnDProvider dndProvider = new DnDProvider();
	ControlDragDrop dnd = new ControlDragDrop();
	
	public void dispose() {
		if (xtable!=null) xtable.dispose();
		xtable = null;
		if (bar!=null) bar.dispose();
		bar = null;
	}
	
	public XChildrenEditor() {
		bar.addCommandBarListener(this);
		xtable.setTableProvider(new XTableProviderImpl());
	}
	
	public void setMnemonicEnabled(boolean b) {
		bar.setMnemonicEnabled(b);
	}

	public void setHeaderVisible(boolean b) {
		xtable.setHeaderVisible(b);
	}
	public Control createControl(Composite parent) {
		control = new Composite(parent, SWT.NONE);
		BorderLayout bl = new BorderLayout();
		control.setLayout(bl);
		xtable.createControl(control);		
		bl.centerComposite = xtable.getControl();
		createCommandBar();
		enableSelectionListener();
		update();
		dnd.setProvider(dndProvider);
		dnd.enable();
		xtable.getTable().addMouseListener(new DoubleClickListener());
		xtable.update();
		return control;	
	}
	
	class DoubleClickListener extends MouseAdapter {
		public void mouseDoubleClick(MouseEvent e) {
			if(bar.isEnabled(EDIT)) action(EDIT);
		}
	}
	
	public Control getControl() {
		return control;
	}
	
	protected int[] getColumnWidthHints() {
		int l = helper.getHeader().length;
		int[] is = new int[l];
		for (int i = 0; i < l; i++) is[i] = 10; 
		return is;
	}

	protected void createCommandBar() {
		String[] commands = (areUpDounActionsEnabled()) 
		         ? new String[]{ADD, EDIT, DELETE, UP, DOWN} 
		         : new String[]{ADD, EDIT, DELETE};
		bar.setCommands(commands);
		bar.getLayout().direction = SWT.VERTICAL;
		bar.getLayout().buttonWidth = convertHorizontalDLUsToPixels(control, IDialogConstants.BUTTON_WIDTH);
		setMargins(bar);
		bar.createControl(control);
		BorderLayout bl = (BorderLayout)control.getLayout();
		bl.eastComposite = bar.getControl();
		bl.eastWidth = SWT.DEFAULT;		
	}
	
	protected void setMargins(CommandBar bar) {
		bar.getLayout().setMargins(0,10,0,0);
	}

	protected int convertHorizontalDLUsToPixels(Control control, int dlus) {
		GC gc= new GC(control);
		gc.setFont(control.getFont());
		int averageWidth= gc.getFontMetrics().getAverageCharWidth();
		gc.dispose();
	
		double horizontalDialogUnitSize = averageWidth * 0.25;
	
		return (int)Math.round(dlus * horizontalDialogUnitSize);
	}
	
	
	protected AbstractTableHelper createHelper() {
		return null;
	}

	public void setObject(XModelObject object) {
		helper.setModelObject(object);
	}
	
	protected boolean areUpDounActionsEnabled() {
		return false;
	}
	
	long updateTimeStamp = -1;

	public void update() {
		if(xtable.getControl() == null || xtable.getControl().isDisposed()) return;
		long ts = (helper.getModelObject() == null) ? -1 : helper.getModelObject().getTimeStamp();
		if(ts != updateTimeStamp || ts == -1) {
			updateTimeStamp = ts;
			lock = true;
			xtable.update();
			lock = false;
		}
		if(selectionListener != null) updateBar();
	}
	
	protected Color getItemColor(int i) {
		return DEFAULT_COLOR;
	}

	protected void enableSelectionListener() {
		selectionListener = new TL();
		xtable.getTable().addSelectionListener(selectionListener);
		updateBar();
	}

	class TL implements SelectionListener {
		public void widgetSelected(SelectionEvent e) {
			onSelectionChanged();
		}
		public void widgetDefaultSelected(SelectionEvent e) {}
	}
	
	protected void onSelectionChanged() {
		if(!lock) {
			updateBar();
			selectionProvider.fireSelectionChanged();
		} 
	}
	
	public void action(String command) {
		if(ADD.equals(command)) add();
		else if(DELETE.equals(command)) delete();
		else if(EDIT.equals(command)) edit();
		else if(UP.equals(command)) up();
		else if(DOWN.equals(command)) down();
		update();
		if(xtable.isActive()) xtable.getTable().setFocus();
	}
	
	protected void add() {
		if(helper.getModelObject() == null) return;
		Set set = getKeys();
		callAction(helper.getModelObject(), getAddActionPath());
		update();
		int i = getAddedKey(set);
		if(i >= 0) {
			xtable.setSelection(i);
			onSelectionChanged();
		}
	}
	
	protected void delete() {
		XModelObject o = helper.getModelObject(xtable.getSelectionIndex());
		if(o != null) callAction(o, "DeleteActions.Delete");
	}
	
	protected void edit() {
		XModelObject o = helper.getModelObject(xtable.getSelectionIndex());
		if(o != null) callAction(o, "Edit");
	}
	
	protected void up() {
		int r1 = xtable.getSelectionIndex(), r2 = r1 - 1;
		if(r2 < 0) return;
		move(r1, r2);
	}
	
	protected void down() {
		int r1 = xtable.getSelectionIndex(), r2 = r1 + 1;
		if(r1 < 0 || r2 >= xtable.getTable().getItemCount()) return;
		move(r1, r2);
	}
	
	private void move(int r1, int r2) {
		XModelObject o1 = helper.getModelObject(r1), o2 = helper.getModelObject(r2);
		callAction(o1, "CopyActions.Copy");
		callAction(o2, "MoveActions.Move");
		if(helper.getModelObject(r2) == o1)
		  xtable.getTable().setSelection(r2);
	}
	
	protected Set getKeys() {
		return xtable.getKeys();
	}

	protected int getAddedKey(Set set) {
		return xtable.getAddedKey(set);
	}

	protected String getAddActionPath() {
		return "CreateActions.AddKeyPair";
	}

	protected void updateBar() {
		boolean enabled = !isReadOnly();
		boolean multi = xtable.getTable().getSelectionIndices().length > 1;
		int r = (!enabled) ? -1 : xtable.getSelectionIndex();
		bar.setEnabled(ADD, enabled);
		bar.setEnabled(DELETE, r >= 0);
		bar.setEnabled(EDIT, r >= 0 && !multi);
		if(areUpDounActionsEnabled()) {
			bar.setEnabled(UP, r > 0 && !multi);
			bar.setEnabled(DOWN, (r >= 0 && r < helper.size() - 1) && !multi);
		}
	}
	
	protected boolean isReadOnly() {
		return (helper == null || helper.getModelObject() == null
		        || !helper.getModelObject().isObjectEditable());
	}
	
	public void callAction(XModelObject o, String path) {
		try {
			Properties p = new Properties();
			p.put("shell", bar.getControl().getShell());
			int i = xtable.getSelectionIndex();
			p.put("insertAfter", new Integer(i));
			XActionInvoker.invoke(path, o, getTargets(), p);
		} catch (Exception e) {
			ModelUIPlugin.log(e);
		}
	}
	
	private XModelObject[] getTargets() {
		int[] is = xtable.getTable().getSelectionIndices();
		if(is.length < 2) return null;
		XModelObject[] os = new XModelObject[is.length];
		for (int i = 0; i < os.length; i++) {
			os[i] = helper.getModelObject(is[i]);
		} 
		return os;		
	}
	
	class XTableProviderImpl implements XTableProvider {

		public int getColumnCount() {
			return helper.getHeader().length;
		}

		public int getRowCount() {
			return helper.size();
		}

		public String getColumnName(int c) {
			return helper.getVisibleHeader()[c];
		}

		public String getValueAt(int r, int c) {
			return helper.getValueAt(r, c);
		}

		public Color getColor(int r) {
			return getItemColor(r);
		}
		
		public int getWidthHint(int c) {
			return getColumnWidthHints()[c];
		}
		
		public Object getDataAt(int r) {
			return helper.getModelObject(r);
		}

		public void dispose() {
		}
	}

	AbstractSelectionProvider selectionProvider = new SP(); 

	public ISelectionProvider getSelectionProvider() {
		return selectionProvider;
	}
	
	public XModelObject getSelectedObject() {
		return ((SP)selectionProvider).getSelectedModelObject();
	}
	
	class SP extends AbstractSelectionProvider {
		protected XModelObject getSelectedModelObject() {
			if(xtable.getTable() == null || xtable.getTable().isDisposed()) return null;
			int r = xtable.getSelectionIndex();
			return helper.getModelObject(r);
		}
		
		protected void setSelectedModelObject(XModelObject object) {
			if(xtable.getTable() == null || xtable.getTable().isDisposed()) return;
			if(object == null || object.getPath() == null) return;
			XModelObject o = helper.getModelObject();
			if(o != null && !object.getPath().startsWith(o.getPath())) return;
			for (int i = 0; i < helper.size(); i++) {
				if(helper.getModelObject(i) == object) {
					xtable.setSelection(i);
					return;
				}
			}
		}
	}
	
	class DnDProvider implements IControlDragDropProvider, IControlDropListener {

		public Control getControl() {
			return xtable.getTable();
		}

		public XModelObject getModelObjectForWidget(Widget widget) {
			if(widget == null) return null;
			Object o = widget.getData();
			return (o instanceof XModelObject) ? (XModelObject)o : null;
		}

		public Widget[] getSelection() {
			return xtable.getTable().getSelection();
		}

		public Properties getDropProperties(int x, int y) {
			return new Properties();
		}

		public void drop(Properties p) {
			XModelObject draggedObject = (XModelObject)p.get("draggedObject");
			if(draggedObject == null) return;
			update();
			xtable.getViewer().setSelection(new StructuredSelection(draggedObject));
		}
		
	}

}
