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
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;

import org.eclipse.core.runtime.IAdaptable;
import org.jboss.tools.common.model.util.XModelTreeListenerSWTASync;
import org.jboss.tools.common.model.ui.IStructuredChangeListener;
import org.jboss.tools.common.model.ui.StructuredChangedEvent;
import org.jboss.tools.common.model.ui.action.sample.XActionWrapper;
import org.jboss.tools.common.model.ui.attribute.editor.TableStructuredEditor;
import org.jboss.tools.common.model.ui.dnd.DnDUtil;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ColumnLayoutData;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.XActionItem;
import org.jboss.tools.common.meta.action.XActionList;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.actions.IActionProvider;

public class XChildrenTableStructuredAdapter implements IAdaptable, ITableAdapter, IModelPropertyEditorAdapter, ISelectionChangedListener, ISelectionProvider {

	public static final String ADD_LABEL = "&Add...";
	public static final String REMOVE_LABEL = "&Remove...";
	public static final String EDIT_LABEL = "&Edit...";
	public static final String UP_LABEL = "&Up";
	public static final String DOWN_LABEL = "&Down";
	
	private static final String COPY_XACTION_PATH = "CopyActions.Copy"; 
	private static final String MOVE_XACTION_PATH = "MoveActions.Move";
	
	private XModelTreeListener modelTreeListener;
	private XModelTreeListenerSWTASync asyncListener;

	private TableColumn[] tableColumn;
	private Table table;
	
	public TableColumn[] createTableColumn(Table table, int style) {
		this.table = table;
		if (tableColumn == null) {
			if ((shownProperties == null) || (shownProperties.length == 0)) {
				shownProperties = new String[] {"name"}; // name attribute as default
				widths = new int[] {100};
			}
			tableColumn = new TableColumn[shownProperties.length];
			ColumnLayoutData layoutData;
			TableLayout layout = new TableLayout();
			for (int i = 0; i < tableColumn.length; ++i) {
				tableColumn[i] = new TableColumn(table, style);
				//tableColumn[i].setResizable(true);
				// column labels
				if ((this.columnLabels!=null)&&(columnLabels.length>i)) {
					tableColumn[i].setText(columnLabels[i]);
				} else {
					tableColumn[i].setText(shownProperties[i]);
				}
				// widths
				layoutData = new ColumnWeightData(widths[i], true);
				layout.addColumnData(layoutData);
				
				//if ((this.widths!=null)&&(widths.length>i)) {
				//	tableColumn[i].setWidth(widths[i]);
				//} else {
				//	tableColumn[i].setWidth(150);
				//}
			}
			table.setLayout(layout);
			
			
			if(tableColumn.length < 4) {
				// Cannot guarantee nice work for many columns (glory) 
				table.addControlListener(new Resize(table));
			}
		}
		return tableColumn;
	}
	
	class Resize extends ControlAdapter {
		private boolean resizeLock = false;
		Table table;
		Resize(Table table) {
			this.table = table;
		}
		public void controlResized(ControlEvent e) {
			updateColumnWidth();
		}
		private void updateColumnWidth() {
			if(resizeLock) return;
			if(table==null || table.getColumnCount() == 0) return;
			resizeLock = true;
			int w = table.getClientArea().width - 1;

			int cw = 0, hs = 0;
			for (int i = 0; i < table.getColumnCount(); i++) {

				cw += table.getColumn(i).getWidth();
				hs += getWidthHint(i);
			}				 
			for (int i = 0; i < table.getColumnCount(); i++) {
				TableColumn c = table.getColumn(i);
				int dw = (w - cw) * getWidthHint(i) / hs;
				try { 
					c.setWidth(c.getWidth() + dw); 



				} catch (Exception exc) {
				}					
			}
			resizeLock = false;
		}		
		protected int getWidthHint(int i) {
			return (widths == null || widths.length < 2 || widths[i] == 0) ? 10 : widths[i];
		}
	}
	
	public Object[] getElements(Object inputElement) {
	    if (value == null) return new Object[0];
		return (Object[])value;
	}

	public void dispose() {
		if (model!=null) {
			model.removeModelTreeListener(asyncListener);
		}
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}

	public Color getForeground(Object element) {
		return null;
	}

	public Color getBackground(Object element) {
		return null;
	}

	// *****************************
	// * IModelPropertyEditorAdapter
	// *****************************
	
	private XModel model;
	private XAttribute attribute;
	private XModelObject xmo;
//	private XAttributeData attributeData;
//	private boolean autoStore;
	private Object value;
	private long[] timeStamps;
	
	public void setModel(XModel model) {
		this.model = model;
	}

	public void setAttribute(XAttribute attribute) {
		this.attribute = attribute;
	}

	public XAttribute getAttribute() {
		return this.attribute;
	}

	public void setModelObject(XModelObject modelObject) {
		xmo = modelObject;
		model = modelObject.getModel();
		value = applyEntitiesFilter(modelObject.getChildren());
		timeStamps = createTimeStamps();
		modelTreeListener = new XMTL();
		asyncListener = new XModelTreeListenerSWTASync(modelTreeListener);
		if (model!=null) {
			model.addModelTreeListener(asyncListener);
		}
		printActionList("",xmo.getModelEntity().getActionList());//.getAction(actionPath);
	}

	public void setAttributeData(XAttributeData attributeData) {
//		this.attributeData = attributeData;
	}
	
	private long[] createTimeStamps() {
		XModelObject[] os = (XModelObject[])value;
		long[] ts = value == null ? new long[0] : new long[os.length];
		for (int i = 0; i < ts.length; i++) ts[i] = os[i].getTimeStamp();
		return ts;
	}	

	public void load() {
	}

	public void store() {
	}

	public void setAutoStore(boolean autoStore) {
//		this.autoStore = autoStore;
	}

	public void addValueChangeListener(PropertyChangeListener l) {
	}

	public void removeValueChangeListener(PropertyChangeListener l) {
	}

	public Object getValue() {
		return this.value;
	}

	public void setValue(Object value) {
		this.value = value;
		this.value = applyEntitiesFilter((XModelObject[])value);
		timeStamps = createTimeStamps();
	}

	public String getStringValue(boolean returnNullAsEmptyString) {
		return (value instanceof String)?(String)value:null;
	}

	public void valueChange(PropertyChangeEvent event) {
	}

	private String[] shownProperties;
	private String[] shownEntities;
	private int[] widths;
	private String[] columnLabels;
	
	private HashMap<String,String> actionMapping = new HashMap<String,String>();
	
	public void setShownProperties(String[] shownProperties) {
		this.shownProperties = shownProperties;
	}
	
	public void setShownEntities(String[] shownEntities) {
		this.shownEntities = shownEntities;
		this.value = applyEntitiesFilter((XModelObject[])value);
		timeStamps = createTimeStamps();
	}
	
	private Object[] applyEntitiesFilter(XModelObject[] children) {
		Object[] result = null;
		if (this.shownEntities == null) return children;
		if (xmo!=null) {
			Set<String> shownEntities = new HashSet<String>();
			if (this.shownEntities!=null) {
				for (int i = 0;i<this.shownEntities.length;++i) shownEntities.add(this.shownEntities[i]);
			}
			if (children!=null) {
				ArrayList<XModelObject> shownChilds = new ArrayList<XModelObject>();
				for (int i = 0;i<children.length;++i) 
					if (shownEntities.contains(children[i].getModelEntity().getName())) 
						shownChilds.add(children[i]);
				result = shownChilds.toArray(new XModelObject[shownChilds.size()]);
			}
		}
		return result;
	}
	
	public void setWidths(int[] is) {
		widths = is;
	}

	public void setColumnLabels(String[] strings) {
		columnLabels = strings;
	}

	// *****************************
	// * IAdaptable
	// *****************************
	public Object getAdapter(Class adapter) {
		if (adapter == IActionProvider.class) {
			return getActionProvider();
		}
		return this;
	}

	// *****************************
	// * ISelectionChangedListener
	// *****************************
//	private SelectionChangedEvent selectionChangedEvent;
	private XModelObject selectedObject;
	
	public void selectionChanged(SelectionChangedEvent event) {
//		this.selectionChangedEvent = event;
		if (event.getSelection() instanceof StructuredSelection) {
			StructuredSelection structuredSelection = (StructuredSelection)event.getSelection();
			Object object = structuredSelection.getFirstElement();
			//if (object instanceof XModelObject) {
				this.selectedObject = (XModelObject)object;
			//}
			//updateButtons();
			actionProvider.setXModelObject(selectedObject);
		}
	}

	private void printActionList(String level, XActionList actionList) {
		if(!ModelUIPlugin.getDefault().isDebugging()) return;

		String actionListName = actionList.getName();
		String actionListDisplayName = actionList.getDisplayName();
		String actionListPath = actionList.getPath();
		System.out.println(level+"ActionList ["+actionListName+"] ["+actionListDisplayName+"] ["+actionListPath+"]");
		XActionItem[] items = actionList.getActionItems();
		for (int i=0;i<items.length;++i) {
			if (items[i] instanceof XActionList) {
				printActionList(level+"    ", (XActionList)items[i]);				
			} else {
				String actionItemName = items[i].getName();
				String actionItemDisplayName = items[i].getDisplayName();
				String actionItemPath = items[i].getPath();
				System.out.println(level+"    "+"ActionItem ["+actionItemName+"] ["+actionItemDisplayName+"] ["+actionItemPath+"]");
			}
		}
	}

	// *****************************
	// * ITableAdapter
	// *****************************
	private ArrayList<IStructuredChangeListener> structureChangeListener = new ArrayList<IStructuredChangeListener>();

	public void addStructureChangeListener(IStructuredChangeListener listener) {
		structureChangeListener.add(listener);
	}

	public void removeStructureChangeListener(IStructuredChangeListener listener) {
		structureChangeListener.remove(listener);
	}

	protected void fireStructureChange() {
		updateValue();
		ArrayList<IStructuredChangeListener> copy = new ArrayList<IStructuredChangeListener>(structureChangeListener);
		Iterator<IStructuredChangeListener> i = copy.iterator();
		while(i.hasNext()){
			i.next().structureChanged(new StructuredChangedEvent(this));
		}
		copy.clear();
		
		if (actionProvider!=null) {
			actionProvider.setXModelObject(selectedObject);
		}
	}
	
	private void updateValue() {
		this.value = applyEntitiesFilter(xmo.getChildren());
		timeStamps = createTimeStamps();
	}

	// *****************************
	// * ISelectionProvider
	// *****************************
	private ArrayList<ISelectionChangedListener> selectionChangeListener = new ArrayList<ISelectionChangedListener>();
	private ISelection selection;

	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		selectionChangeListener.add(listener);
	}

	public ISelection getSelection() {
		selection = new StructuredSelection(new Object[] {this.selectedObject}); 
		return selection;
	}

	public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		selectionChangeListener.remove(listener);
	}

	public void setSelection(ISelection selection) {
		this.selection = selection;
	}

	protected void fireSelectionChange() {
		ArrayList<ISelectionChangedListener> copy = new ArrayList<ISelectionChangedListener>(selectionChangeListener);
		Iterator<ISelectionChangedListener> i = copy.iterator();
		while(i.hasNext()){
			i.next().selectionChanged(new SelectionChangedEvent(this, getSelection()));
		}
		copy.clear();
	}

	class AddAction extends XActionWrapper {
		public AddAction(String label) {
			super(label);
		}
		public void run() {
			super.run();
			fireStructureChange();
		}
		protected void initRunningProperties(Properties p) {
			
		}

	}
	class RemoveAction extends XActionWrapper {
		public RemoveAction(String label) {
			super(label);
		}
		public void run() {
			super.run();
			fireStructureChange();
		}
	}
	
	class EditAction extends XActionWrapper {
		public EditAction() {}
		public EditAction(String label) {
			super(label);
		}
		public void setActionPath(String path) {
			this.path = path;
			if ("%SelectIt%".equals(path)) {
				if (xmo != null) {
					setEnabled(true);
				} else { 
					setEnabled(false);
				}
				return;
			} 
			if ((xmo == null) || (path == null)) {
				setEnabled(false);
			} else {
				setEnabled(DnDUtil.getEnabledAction(this.xmo, null, path) != null);
			}
		}
		public void setXModelObject(XModelObject xmo) {
			this.xmo = xmo;
			if ("%SelectIt%".equals(path)) {
				if (xmo != null) {
					setEnabled(true);
				} else { 
					setEnabled(false);
				}
				return;
			} 
			if ((xmo == null) || (path == null)) {
				setEnabled(false);
			} else {
				setEnabled(xmo.getModelEntity().getActionList().getAction(path)!=null);
			}
		}
		
		public void run() {
			if ("%SelectIt%".equals(this.path)) {
				if (selectionChangedListener!=null) {
					if(this.xmo == null) return;
					ISelection selection = new StructuredSelection(this.xmo);
					SelectionChangedEvent event = new SelectionChangedEvent(new SelectionProvider(selection), selection); 
					selectionChangedListener.selectionChanged(event);
				}
			} else {
				super.run();
			}
			fireStructureChange();
		}
		
	}

	class SelectionProvider implements ISelectionProvider {
		ISelection selection;
		
		public SelectionProvider(ISelection selection) {
			this.selection = selection;
		}
		public void addSelectionChangedListener(ISelectionChangedListener listener) {
		}
		public ISelection getSelection() {
			return selection;
		}
		public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		}
		public void setSelection(ISelection selection) {
		}
	}
	
	class MoveAction extends XActionWrapper {
		public MoveAction(String label) {
			super(label);
		}
		public void setActionPath(String path) {}

		public void setXModelObject(XModelObject xmo) {
			XModelObject[] list = (XModelObject[])XChildrenTableStructuredAdapter.this.value;
			this.xmo = xmo;
			if (xmo!=null) {
				int index = getIndex(list, xmo);
				int targetIngex = getTargetIndex(index);
				boolean actionEnabled = ((xmo.getModelEntity().getActionList().getAction(COPY_XACTION_PATH)!=null)&&(xmo.getModelEntity().getActionList().getAction(MOVE_XACTION_PATH)!=null));
				setEnabled(actionEnabled && (targetIngex >= 0 && targetIngex < list.length) && xmo.isObjectEditable());
			} else {
				setEnabled(false);
			}
		}
		
		private int getIndex(XModelObject[] list, XModelObject child) {
			for (int i = 0; i < list.length; ++i) {
				if (child.equals(list[i])) return i;
			}
			return -1;
		}
		
		public void run() {
			XModelObject[] list = (XModelObject[])XChildrenTableStructuredAdapter.this.value;
			int index = getIndex(list, this.xmo);
			int targetIndex = getTargetIndex(index);
			if(index == targetIndex || targetIndex < 0 || targetIndex >= list.length) return;
			try {
				XActionInvoker.invoke(COPY_XACTION_PATH, this.xmo, null, new Properties());
			} catch (Exception e) {
				ModelUIPlugin.log(e);
			}
			XModelObject prev = list[targetIndex];
			try {
				XActionInvoker.invoke(MOVE_XACTION_PATH, prev, null, new Properties());
				if(table != null && !table.isDisposed()) table.setSelection(targetIndex);
			} catch (Exception e) {
				ModelUIPlugin.log(e);
			}
			fireStructureChange();
		}
		protected int getTargetIndex(int index) {
			return index;
		}
	}

	class MoveUpAction extends MoveAction {
		public MoveUpAction() {
			super(UP_LABEL);
		}
		protected int getTargetIndex(int index) {
			return index - 1;
		}
	}
	
	class MoveDownAction extends MoveAction {
		public MoveDownAction() {
			super(DOWN_LABEL);
		}
		protected int getTargetIndex(int index) {
			return index + 1;
		}
	}
	
	class ActionProvider implements IActionProvider {
		private ArrayList<IAction> actions = new ArrayList<IAction>();
		private HashMap<String,IAction> hash = new HashMap<String,IAction>();
		private boolean showUpDown = true;
	
		XActionWrapper addAction = new AddAction(ADD_LABEL); 
		XActionWrapper removeAction = new RemoveAction(REMOVE_LABEL); 
		XActionWrapper editAction = new EditAction(EDIT_LABEL); 
		XActionWrapper upAction = new MoveUpAction(); 
		XActionWrapper downAction = new MoveDownAction(); 
		
		private ActionProvider() {}
	
		public ActionProvider(boolean makeDefaultActions, boolean showUpDown) {
			this.showUpDown = showUpDown;

			if (makeDefaultActions) {
				actions.add(addAction);
				actions.add(removeAction);
				actions.add(editAction);
				if (this.showUpDown) {
					actions.add(upAction);
					actions.add(downAction);
				}
				
				hash.put(TableStructuredEditor.ADD_ACTION, addAction);
				hash.put(TableStructuredEditor.REMOVE_ACTION, removeAction);
				hash.put(TableStructuredEditor.EDIT_ACTION, editAction);
				if (this.showUpDown) {
					hash.put(TableStructuredEditor.UP_ACTION, upAction);
					hash.put(TableStructuredEditor.DOWN_ACTION, downAction);
				}
				hash.put(TableStructuredEditor.DOUBLE_CLICK__ACTION, editAction);
			}
		}
		
		public void update(ISelection selection) {
			removeAction.setEnabled(!selection.isEmpty());
			editAction.setEnabled(!selection.isEmpty());
		}
		public void setXModelObject(XModelObject xmo) {
			// add action
			addAction.setXModelObject(XChildrenTableStructuredAdapter.this.xmo);
			addAction.setActionPath((String)actionMapping.get(TableStructuredEditor.ADD_ACTION));
			
			removeAction.setXModelObject(xmo);
			removeAction.setActionPath((String)actionMapping.get(TableStructuredEditor.REMOVE_ACTION));

			editAction.setXModelObject(xmo);
			editAction.setActionPath((String)actionMapping.get(TableStructuredEditor.EDIT_ACTION));
			
			if (this.showUpDown) {
				upAction.setXModelObject(xmo);
				upAction.setActionPath((String)actionMapping.get(TableStructuredEditor.UP_ACTION));

				downAction.setXModelObject(xmo);
				downAction.setActionPath((String)actionMapping.get(TableStructuredEditor.DOWN_ACTION));
			}
		}
		
		public IAction getAction(String actionName) {
			return (IAction)hash.get(actionName);
		}

		public IAction[] getActions() {
			return (IAction[])actions.toArray(new IAction[actions.size()]);
		}
	}

	// *****************************
	// * IActionProvider
	// *****************************
	
	private boolean makeDefaultActions = true;
	
	private ActionProvider actionProvider;
	
	private boolean isCanBeMoveChildren() {
		if (shownEntities != null && shownEntities.length > 0 && xmo != null) {
			XModelEntity entity = xmo.getModel().getMetaData().getEntity(shownEntities[0]);
			Object copyXAction = entity.getActionList().getAction(COPY_XACTION_PATH);
			Object moveXAction = entity.getActionList().getAction(MOVE_XACTION_PATH);
			if (copyXAction != null && moveXAction != null) {
				return true;
			}
		}
		return false;
	}
	
	private void initializeActionProvider() {
		actionProvider = new ActionProvider(this.makeDefaultActions, this.isCanBeMoveChildren());
		actionProvider.setXModelObject(this.selectedObject);
	}

	public IActionProvider getActionProvider() {
		if (this.actionProvider==null) initializeActionProvider();
		return actionProvider;
	}

	public HashMap<String,String> getActionMapping() {
		return actionMapping;
	}
	
	// ISelectionChangedListener
	private ISelectionChangedListener selectionChangedListener;

	public ISelectionChangedListener getSelectionChangedListener() {
		return selectionChangedListener;
	}

	public void setSelectionChangedListener(ISelectionChangedListener listener) {
		selectionChangedListener = listener;
	}

	private boolean equals(long[] array1, long[] array2) {
		if (array1 == null && array2 == null) return true;
		if (array1 != null && array2 != null) {
			if (array1.length != array2.length) return false;
			for (int i=0;i<array1.length;++i) {
				if(array1[i] != array2[i]) return false;
			}
			return true;
		} else { 
			return false;
		}
	} 
	
	private void update() {
		Object[] newValue = applyEntitiesFilter(xmo.getChildren());
		long[] ts = createTimeStamps();
		if (!equals(timeStamps, ts) || !areArraysEqual((Object[])value, newValue)) {
			value = newValue;
			timeStamps = ts;
			fireStructureChange();
		}
	}
	
	private boolean areArraysEqual(Object[] os1, Object[] os2) {
		if(os1 == null && os2 == null) return true;
		if(os1 != null && os2 != null && os1.length == os2.length) {
			for (int i = 0; i < os1.length; i++) {
				if(os1[i] != os2[i]) return false;
			}
			return true;
		}
		return false;
		
	}

	// XModelTreeListener
	class XMTL implements XModelTreeListener {
		public void nodeChanged(XModelTreeEvent event){
			String opath = (String)event.getInfo();
			String npath = event.getModelObject().getPath();
			if(!opath.equals(npath)) {
			   // update
			}
			if (xmo == null) return;
			String rootPath = xmo.getPath();
			String eventPath = event.getModelObject().getPath();
			if ((rootPath == null) || (eventPath == null) || (!eventPath.startsWith(rootPath))) return;
			update();
		}
   	 
		public void structureChanged(XModelTreeEvent event) {
			if (xmo == null) return;
			String rootPath = xmo.getPath();
			String eventPath = event.getModelObject().getPath();
			if ((rootPath==null)||(eventPath==null)||(!eventPath.startsWith(rootPath))) return;
			if(event.kind()==XModelTreeEvent.STRUCTURE_CHANGED) {
				update();
			} else if(event.kind()==XModelTreeEvent.CHILD_ADDED) {
				update();
				XModelObject addedObject = (XModelObject)event.getInfo();
				if (addedObject!=null) selectedObject = addedObject;
				fireSelectionChange();
				
			} else if(event.kind()==XModelTreeEvent.CHILD_REMOVED) {
				update();
		   }
		}
	}
	
	private ITableLabelProvider tableLabelProvider = new DefaultTableLabelProvider();
	
	public ITableLabelProvider getTableLabelProvider() {
		return tableLabelProvider;
	}

	public void setTableLabelProvider(ITableLabelProvider tableLabelProvider) {
		this.tableLabelProvider = tableLabelProvider;
	}

	class DefaultTableLabelProvider implements ITableLabelProvider {

		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
		 */
		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
		 */
		public String getColumnText(Object element, int columnIndex) {
			XModelObject xmo = ((XModelObject)element);
			String result = xmo.getAttributeValue(shownProperties[columnIndex]); 
			return (result!=null)?result:"no attribute called "+shownProperties[columnIndex]+" on entity "+xmo.getModelEntity().getName();
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
		 */
		public void addListener(ILabelProviderListener listener) {
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
		 */
		public void dispose() {
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
		 */
		public boolean isLabelProperty(Object element, String property) {
			return true;
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
		 */
		public void removeListener(ILabelProviderListener listener) {
		}
	}

	public boolean hasErrors() {
		return false;
	}
    public boolean isMakeDefaultActions() {
        return makeDefaultActions;
    }
    public void setMakeDefaultActions(boolean makeDefaultActions) {
        this.makeDefaultActions = makeDefaultActions;
    }

	public String getError() {
		return null;
	}
}
