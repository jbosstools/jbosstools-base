package org.jboss.tools.common.model.ui.test;

import java.util.Properties;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.navigator.decorator.DecoratorManager;
import org.jboss.tools.common.model.ui.navigator.decorator.XModelObjectDecorator;

import junit.framework.TestCase;

public class ObjectDecoratorTest extends TestCase {
	static String ENTITY = "FileTLD_1_2";
	static String ATTR_NAME = "name";
	static String ATTR_DISPLAY = "display-name";
	static String ATTR_URI = "uri";
	static String ATTR_SHORTNAME = "shortname";

	String[][] tests = {
		{"{display-name}", "Taglib File"},
		{"{name} - {uri}", "taglibFile.tld - http:/a.b.c/d"},
		{"{name} : {shortname}", "taglibFile.tld : s"},
		{"{name} -> {short-name}", "taglibFile.tld -> {short-name}"},
	};
	
	public void testObjectDecorator() {
		XModelObject o = PreferenceModelUtilities.getPreferenceModel().createModelObject(ENTITY, new Properties());
		assertNotNull("Cannot create object for entity " + ENTITY, o);
		o.setAttributeValue(ATTR_NAME, "taglibFile");
		o.setAttributeValue(ATTR_DISPLAY, "Taglib File");
		o.setAttributeValue(ATTR_URI, "http:/a.b.c/d");
		o.setAttributeValue(ATTR_SHORTNAME, "s");
		
		XModelObjectDecorator d = DecoratorManager.getInstance().getDecoratorByEntity(ENTITY);
		assertNotNull("Cannot find decorator for " + ENTITY, d);
		
		for (int i = 0; i < tests.length; i++) {
			d.setValue(tests[i][0]);
			String label = d.getLabel(o);
			assertEquals("Unexpected label for format " + tests[i][0], tests[i][1], label);
		}
	}

}
