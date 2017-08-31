package org.jboss.tools.runtime.reddeer.wizard;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.hamcrest.Matcher;
import org.eclipse.reddeer.common.wait.WaitUntil;
import org.eclipse.reddeer.core.condition.WidgetIsFound;
import org.eclipse.reddeer.core.matcher.WithMnemonicTextMatcher;
import org.eclipse.reddeer.core.matcher.WithStyleMatcher;
import org.eclipse.reddeer.core.reference.ReferencedComposite;
import org.eclipse.reddeer.jface.wizard.WizardPage;
import org.eclipse.reddeer.swt.impl.button.RadioButton;

public class TaskWizardSecondPage extends WizardPage{

	public TaskWizardSecondPage(ReferencedComposite referencedComposite) {
		super(referencedComposite);
	}

	public void acceptLicense(boolean accept){
		if(accept){
			new WaitUntil(new WidgetIsFound(Button.class, getRadioButtonMatchers("I accept the terms of the license agreement")));
			new RadioButton("I accept the terms of the license agreement").click();
		}else {
			new RadioButton("I do not accept the terms of the license agreement").click();
		}
	}
	
	private Matcher<?>[] getRadioButtonMatchers(String text){
		List<Matcher<?>> list= new ArrayList<Matcher<?>>();
		list.add(new WithStyleMatcher(SWT.RADIO));
		list.add(new WithMnemonicTextMatcher(text));
		return list.toArray(new Matcher[list.size()]);
	}
	
}
