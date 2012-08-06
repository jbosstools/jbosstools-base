package org.jboss.tools.ui.bot.ext.parts;

import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swtbot.swt.finder.ReferenceBy;
import org.eclipse.swtbot.swt.finder.SWTBotWidget;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.results.BoolResult;
import org.eclipse.swtbot.swt.finder.results.StringResult;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBotControl;
import org.eclipse.ui.forms.widgets.Twistie;
import org.hamcrest.SelfDescribing;

@SWTBotWidget(clasz = Twistie.class, preferredName="twistie", referenceBy = { ReferenceBy.LABEL})
public class SWTBotTwistie extends AbstractSWTBotControl<Twistie>{	

	public SWTBotTwistie(Twistie w) throws WidgetNotFoundException {
		super(w);
	}

	public SWTBotTwistie(Twistie w, SelfDescribing description)
			throws WidgetNotFoundException {
		super(w, description);
	}
	/**
	 * Returns text of the first sibling label
	 * @return text of the first sibling Label (or CLabel) or null if there is no Label
	 */
	
	public String getLabelText(){
		return syncExec(new StringResult() {		
			@Override
			public String run() {
				Control[] aux = widget.getParent().getChildren();
				for (Control control : aux) {
					if (control instanceof CLabel){
						return ((CLabel)control).getText();
					}
					if (control instanceof Label){
						return ((Label)control).getText();
					}
				}
				return null;
			}
		});
	}
	
	/**
	 * Toggles twistie (expands its section)
	 */
	public AbstractSWTBotControl<Twistie> toggle() {
		setFocus();
		keyboard().typeCharacter('\r');
		return this;
	}
	
	
	/**
	 * Checks whether the Twistie is expanded
	 * @return true if Twistie is expanded
	 */
	public boolean isExpanded(){
		return syncExec(new BoolResult() {
			
			@Override
			public Boolean run() {
				return widget.isExpanded();
			}
		});
	}

}
