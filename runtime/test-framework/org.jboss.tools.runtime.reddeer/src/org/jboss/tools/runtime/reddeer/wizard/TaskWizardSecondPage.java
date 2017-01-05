package org.jboss.tools.runtime.reddeer.wizard;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.hamcrest.Matcher;
import org.jboss.reddeer.common.wait.WaitUntil;
import org.jboss.reddeer.core.condition.WidgetIsFound;
import org.jboss.reddeer.core.matcher.ClassMatcher;
import org.jboss.reddeer.core.matcher.WithMnemonicTextMatcher;
import org.jboss.reddeer.core.matcher.WithStyleMatcher;
import org.jboss.reddeer.jface.wizard.WizardPage;
import org.jboss.reddeer.swt.impl.button.RadioButton;

public class TaskWizardSecondPage extends WizardPage{

	public void acceptLicense(boolean accept){
		if(accept){
			new WaitUntil(new WidgetIsFound<Button>(getRadioButtonMatchers("I accept the terms of the license agreement")));
			new RadioButton("I accept the terms of the license agreement").click();
		}else {
			new RadioButton("I do not accept the terms of the license agreement").click();
		}
	}
	
	private Matcher<?>[] getRadioButtonMatchers(String text){
		List<Matcher<?>> list= new ArrayList<Matcher<?>>();
		list.add(new WithStyleMatcher(SWT.RADIO));
		list.add(new WithMnemonicTextMatcher(text));
		list.add(new ClassMatcher(Button.class));
		return list.toArray(new Matcher[list.size()]);
	}
	
}
