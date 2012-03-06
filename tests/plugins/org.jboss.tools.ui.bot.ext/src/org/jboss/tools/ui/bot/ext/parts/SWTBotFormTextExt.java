package org.jboss.tools.ui.bot.ext.parts;

import java.lang.reflect.Field;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swtbot.swt.finder.ReferenceBy;
import org.eclipse.swtbot.swt.finder.SWTBotWidget;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.results.StringResult;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBotControl;
import org.eclipse.ui.forms.widgets.FormText;
import org.eclipse.ui.internal.forms.widgets.FormTextModel;
import org.hamcrest.SelfDescribing;

@SWTBotWidget(clasz = FormText.class, preferredName="formText", referenceBy = { ReferenceBy.MNEMONIC })
public class SWTBotFormTextExt extends AbstractSWTBotControl<FormText>{

	public SWTBotFormTextExt(FormText w) throws WidgetNotFoundException {
		super(w);
	}

	public SWTBotFormTextExt(FormText w, SelfDescribing description)
			throws WidgetNotFoundException {
		super(w, description);
	}
	
	public String selectionText() {
		return syncExec(new StringResult() {
			
			@Override
			public String run() {
				return widget.getSelectionText();
			}
		});
	}
	
	public AbstractSWTBotControl<FormText> click()  {
		setFocus();
		keyboard().typeCharacter('\r');
		return this;
	}
	
	public String selectedLinkText() {
		return syncExec(new StringResult() {
			
			@Override
			public String run() {
				return widget.getSelectedLinkText();
			}
		});
	}
	
	public String toolTipText() {
		return syncExec(new StringResult() {
			
			@Override
			public String run() {
				return widget.getToolTipText();
			}
		});
	}

	@Override
	@SuppressWarnings("restriction")
	public String getText() {
		Field field;
		try {
			field = widget.getClass().getDeclaredField("model");
		} catch (SecurityException e1) {
			throw new SecurityException(e1);
		} catch (NoSuchFieldException e1) {
			throw new SecurityException(e1);
		}
		FormTextModel model;
		field.setAccessible(true);
		try {
			model = (FormTextModel)field.get(widget);
		} catch (IllegalArgumentException e1) {
			throw new SecurityException(e1);
		} catch (IllegalAccessException e1) {
			throw new SecurityException(e1);
		}
		return model.getAccessibleText().trim();
		
	}
	

}
