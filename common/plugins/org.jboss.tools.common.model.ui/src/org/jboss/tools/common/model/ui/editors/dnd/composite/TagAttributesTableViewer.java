package org.jboss.tools.common.model.ui.editors.dnd.composite;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationEvent;
import org.eclipse.jface.viewers.ICellEditorListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.jface.viewers.ViewerRow;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

public class TagAttributesTableViewer extends TableViewer {
	private boolean isHandle = true;
	private boolean isTraversed = false;

	public TagAttributesTableViewer(Composite parent, int style) {
		super(parent, style);
	}

	public TagAttributesTableViewer(Composite parent) {
		super(parent);
	}

	public TagAttributesTableViewer(Table table) {
		super(table);
	}

	@Override
	protected void hookControl(Control control) {
		super.hookControl(control);
		if (getColumnViewerEditor() != null) {
			getTable().addKeyListener(new KeyListener() {

				public void keyReleased(KeyEvent e) {
					if (isHandle || (!isHandle && isTraversed)) {
						handleKeyReleasedEvent(e);
					}
					isHandle = true;
					isTraversed = false;
				}

				public void keyPressed(KeyEvent e) {
				}
			});
			addSelectionChangedListener(new ISelectionChangedListener() {
				public void selectionChanged(SelectionChangedEvent event) {
					isTraversed = false;
					isHandle = true;
				}
			});
			getTable().addTraverseListener(new TraverseListener() {

				public void keyTraversed(TraverseEvent e) {
					if (e.character == '\r') {
						e.doit = false;
					}
					if (isCellEditorActive() && e.detail == SWT.TRAVERSE_ESCAPE) {
						e.doit = false;
						e.detail = SWT.TRAVERSE_NONE;
						isTraversed = true;
					}
				}
			});
		}
	}

	private void handleKeyReleasedEvent(KeyEvent keyEvent) {
		if (keyEvent.character == '\r' && keyEvent.doit == true) {
			TableItem[] selectedItems = getTable().getSelection();
			if (selectedItems == null || selectedItems.length != 1) {
				return;
			}
			TableItem item = selectedItems[0];
			ViewerRow row = getViewerRowFromItem(item);
			ViewerCell cell = row.getCell(1);
			if (cell != null) {
				triggerEditorActivationEvent(new ColumnViewerEditorActivationEvent(
						cell));
			}
		}
	}

	@Override
	public void setCellEditors(CellEditor[] editors) {
		super.setCellEditors(editors);
		editors[1].addListener(new ICellEditorListener() {

			public void editorValueChanged(boolean oldValidState,
					boolean newValidState) {
				isHandle = false;
			}

			public void cancelEditor() {
				isHandle = false;
			}

			public void applyEditorValue() {
			}
		});
	}

}
