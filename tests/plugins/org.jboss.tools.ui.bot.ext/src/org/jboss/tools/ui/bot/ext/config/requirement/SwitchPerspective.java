package org.jboss.tools.ui.bot.ext.config.requirement;

import java.util.List;
import java.util.Vector;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.gen.IPerspective;

public class SwitchPerspective extends RequirementBase {

	private final String name;
	public SwitchPerspective(String name) {
		this.name = name;
	}
	
	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.bot.activePerspective().getLabel().equals(name);
	}

	@Override
	public void handle() {
		SWTTestExt.open.perspective(new IPerspective() {
			public List<String> getGroupPath() {
				return new Vector<String>();
			}
			public String getName() {
				return name;
			}});

	}

}
