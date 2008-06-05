package org.jboss.tools.common.model.ui.attribute.editor;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Slider;

public class SliderCellEditorEx extends CellEditor {

	private Slider slider = null;
	private int style = SWT.HORIZONTAL;
	private static final int MAX_SLIDER_VALUE = 1000;
	private static final int MIN_SLIDER_VALUE = 0;
	private static final int INCREMENT_SLIDER_VALUE = 10;
	private static final int DEFAULT_SLIDER_VALUE = 500;
	public SliderCellEditorEx() {
		super();
	}

	public SliderCellEditorEx(Composite parent) {
		super(parent);
	}

	public SliderCellEditorEx(Composite parent, int style) {
		super(parent, style);
		this.style = style;
	}

	@Override
	protected Control createControl(Composite parent) {
		slider = new Slider(parent, this.style);
		slider.setMaximum(MAX_SLIDER_VALUE);
		slider.setMinimum(MIN_SLIDER_VALUE);
		slider.setIncrement(INCREMENT_SLIDER_VALUE);
		slider.setSelection(DEFAULT_SLIDER_VALUE);
		return slider;
	}

	@Override
	protected Object doGetValue() {
		if (null != slider) {
			slider.getSelection();
		}
		return null;
	}

	@Override
	protected void doSetFocus() {
		if (null != slider) {
			slider.setFocus();
		}
	}

	@Override
	protected void doSetValue(Object value) {
		try {
			int intValue = Integer.parseInt(value.toString());
			if (intValue <= MAX_SLIDER_VALUE && intValue >= MIN_SLIDER_VALUE) {
				slider.setSelection(intValue);
			}
		} catch (Exception e) {
			// Do nothing
		}
	}

}
