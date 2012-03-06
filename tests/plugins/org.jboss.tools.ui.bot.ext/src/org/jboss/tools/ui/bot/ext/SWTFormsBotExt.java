package org.jboss.tools.ui.bot.ext;

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swtbot.forms.finder.SWTFormsBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.ui.forms.widgets.FormText;
import org.jboss.tools.ui.bot.ext.parts.SWTBotFormTextExt;


/**
 * Extended version of SWTFormsBot with formText recognition capabilities
 * 
 * 
 * @author rhopp
 *
 */

public class SWTFormsBotExt extends SWTFormsBot{

	public SWTBotFormTextExt formText(){
		return formText(0);
	}

	@SuppressWarnings("unchecked")
	public SWTBotFormTextExt formText(int index){
		try{
			List<FormText> formTexts = (List<FormText>) widgets(widgetOfType(FormText.class));
			return new SWTBotFormTextExt(formTexts.get(index));
		}catch(WidgetNotFoundException ex){
			throw new WidgetNotFoundException("Could not find widget of type FormText", ex);
		}catch(ArrayIndexOutOfBoundsException ex){
			throw new ArrayIndexOutOfBoundsException("There is widget Form Text with such a index");
		}
	}
	
	public SWTBotFormTextExt formTextWithText(String text){
		return formTextWithText(text, 0);
	}
	
	@SuppressWarnings("unchecked")
	public SWTBotFormTextExt formTextWithText(String text, int index){
		try{
			List<SWTBotFormTextExt> formTextBots = new ArrayList<SWTBotFormTextExt>();
			List<FormText> formTexts = (List<FormText>) widgets(widgetOfType(FormText.class));
			for (FormText formText : formTexts) {
				SWTBotFormTextExt auxBotFormTextExt = new SWTBotFormTextExt(formText);
				String textik = auxBotFormTextExt.getText();
				if (textik.equals(text)){
					formTextBots.add(auxBotFormTextExt);
				}
			}
			return formTextBots.get(index);
		}catch(WidgetNotFoundException ex){
			throw new WidgetNotFoundException("Could not find widget of type FormText", ex);
		}catch(IndexOutOfBoundsException ex){
			throw new ArrayIndexOutOfBoundsException("There is no widget Form Text with text: "+text+" and index: "+index);
		}
	}

}
