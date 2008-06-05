package org.jboss.tools.common.model.ui.attribute.editor;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.widgets.Composite;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class SliderEditor extends ValueEditor {

	public SliderEditor() {}
	
	public SliderEditor(IWidgetSettings settings) {
		super(settings);
		lineEditor = true;
	}
	
	public void dispose() {
		super.dispose();
		if (cellEditor != null) {
			cellEditor.dispose();
		}
		cellEditor = null;
		if (fieldEditor != null) {
			fieldEditor.dispose();
		}
		fieldEditor = null;
	}
	
	@Override
	protected CellEditor createCellEditor(Composite parent) {
		cellEditor = new SliderCellEditorEx();
		return cellEditor;
	}

	@Override
	protected ExtendedFieldEditor createFieldEditor(Composite parent) {
		fieldEditor = new SliderFieldEditor();
		fieldEditor.setLabelText(getLabelText());
		return fieldEditor;
	}

}
