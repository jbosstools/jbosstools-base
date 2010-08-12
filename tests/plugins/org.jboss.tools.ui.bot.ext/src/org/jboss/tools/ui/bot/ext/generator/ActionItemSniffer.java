/*******************************************************************************
 * Copyright (c) 2007-2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.ui.bot.ext.generator;

import java.util.Iterator;
import java.util.List;
import java.util.Stack;
import java.util.Vector;
import org.junit.BeforeClass;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(SWTBotJunit4ClassRunner.class)
public class ActionItemSniffer extends SWTTestExt {

	private static SWTWorkbenchBot bot;

	@Test
	public void generateLabels() {

		Stack<SWTBotTreeItem> stack = new Stack<SWTBotTreeItem>();

		// retrieve createnew tree items
		List<LabelEntity> createNew = new Vector<LabelEntity>();

		bot.menu("File").menu("New").menu("Other...").click();
		SWTBotShell shell = bot.shell("New");
		shell.activate();

		for (SWTBotTreeItem item : bot.tree().getAllItems()) {
			getItems(item, stack, createNew, true);
		}
		bot.button("Cancel").click();
		stack.clear();

		// retrieve perspectives
		List<LabelEntity> perspectives = new Vector<LabelEntity>();
		bot.menu("Window").menu("Open Perspective").menu("Other...").click();
		shell = bot.shell("Open Perspective");
		shell.activate();
		for (int i = 0; i < bot.table().rowCount(); i++) {
			LabelEntity le = new LabelEntity();
			le.setName(bot.table().getTableItem(i).getText());
			perspectives.add(le);
		}
		bot.button("Cancel").click();
		stack.clear();

		// retrieve views
		List<LabelEntity> views = new Vector<LabelEntity>();
		bot.menu("Window").menu("Show View").menu("Other...").click();
		shell = bot.shell("Show View");
		shell.activate();
		for (SWTBotTreeItem item : bot.tree().getAllItems()) {
			getItems(item, stack, views, true);
		}
		bot.button("Cancel").click();
		stack.clear();

		// retrieve Import options
		List<LabelEntity> imports = new Vector<LabelEntity>();
		bot.menu("File").menu("Import...").click();
		shell = bot.shell("Import");
		shell.activate();
		for (SWTBotTreeItem item : bot.tree().getAllItems()) {
			getItems(item, stack, imports, true);
		}
		bot.button("Cancel").click();
		stack.clear();

		// retrieve exports
		List<LabelEntity> exports = new Vector<LabelEntity>();
		bot.menu("File").menu("Export...").click();
		shell = bot.shell("Export");
		shell.activate();
		for (SWTBotTreeItem item : bot.tree().getAllItems()) {
			getItems(item, stack, exports, true);
		}
		bot.button("Cancel").click();
		stack.clear();

		// retrieve preferences tree
		List<LabelEntity> preferences = new Vector<LabelEntity>();
		bot.menu("Window").menu("Preferences").click();
		shell = bot.shell("Preferences");
		shell.activate();
		for (SWTBotTreeItem item : bot.tree().getAllItems()) {
			getItems(item, stack, preferences, false);
		}
		bot.button("Cancel").click();
		stack.clear();
		
		List<LabelEntity> serverRuntimes = new Vector<LabelEntity>();
		bot.menu("Window").menu("Preferences").click();
		shell = bot.shell("Preferences");
		shell.activate();
		bot.tree().expandNode("Server").expandNode("Runtime Environments").select();
		bot.button("Add...").click();
		shell = bot.shell("New Server Runtime Environment");
		shell.activate();
		for (SWTBotTreeItem item : bot.tree().getAllItems()) {
			getItems(item, stack, serverRuntimes, true);
		}
		fillEntityDetails(serverRuntimes, true);
		bot.button("Cancel").click();
		bot.button("Cancel").click();
		stack.clear();
		
		List<LabelEntity> servers = new Vector<LabelEntity>();
		bot.menu("File").menu("New").menu("Other...").click();
		shell = bot.shell("New");
		shell.activate();
		bot.tree().expandNode("Server").select("Server");
		bot.button("Next >").click();
		for (SWTBotTreeItem item : bot.tree().getAllItems()) {
			getItems(item, stack, servers, true);
		}
		fillEntityDetails(servers, true);
		bot.button("Cancel").click();
		stack.clear();
		
		
		// switch to java perspective
		bot.menu("Window").menu("Open Perspective").menu("Other...").click();
		shell = bot.shell("Open Perspective");
		shell.activate();
		bot.table().select("Java");
		bot.button("OK").click();
		// first create some project
		bot.menu("File").menu("New").menu("Other...").click();
		shell = bot.shell("New");
		shell.activate();
		bot.tree().getTreeItem("Java Project").select();
		bot.button("Next >").click();
		bot.textWithLabel("Project name:").setText("aaa");
		bot.sleep(10000);
		bot.button("Finish").click();
		bot.sleep(20000);
		// go through all wizards, click next, sniff labels and back
		bot.menu("File").menu("New").menu("Other...").click();
		shell = bot.shell("New");
		shell.activate();
		fillEntityDetails(createNew, true);
		bot.button("Cancel").click();

		bot.menu("File").menu("Import...").click();
		shell = bot.shell("Import");
		shell.activate();
		fillEntityDetails(imports, true);
		bot.button("Cancel").click();

		bot.menu("File").menu("Export...").click();
		shell = bot.shell("Export");
		shell.activate();
		fillEntityDetails(exports, true);
		bot.button("Cancel").click();

		// finally generate java sources
		ActionItemWriter gen = new ActionItemWriter();
		gen.getEntityMap().put("View", views);
		gen.getEntityMap().put("Server", servers);
		gen.getEntityMap().put("Perspective", perspectives);
		gen.getEntityMap().put("NewObject", createNew);
		gen.getEntityMap().put("Import", imports);
		gen.getEntityMap().put("Export", exports);
		gen.getEntityMap().put("Preference", preferences);
		gen.getEntityMap().put("ServerRuntime", serverRuntimes);
		try {
			gen.generateInterfaces();
			gen.generateClasses();
		} catch (Exception e) {
			log.error(e);
		}

	}

	private void fillEntityDetails(List<LabelEntity> createNew, boolean mustNext) {
		for (final LabelEntity le : createNew) {
			// filter some items
			if (le.getName().equals("Create a Sample Web Service")) {
				continue;
			}
			if (le.getPath().size() > 0 && le.getPath().get(0).equals("Test")) {
				continue;
			}
			log.info("Scanning " + le.toString());
			Iterator<String> iter = le.getPath().iterator();

			SWTBotTreeItem item = bot.tree().getTreeItem(iter.next());
			item.expand();
			try {

				while (iter.hasNext()) {
					String nodeName = iter.next();
					item = item.getNode(nodeName);
					item.expand();
				}

			} catch (WidgetNotFoundException ex) {
				log.error("Error processing treeView, skipping, item "
						+ le.toString());
				log.error(ex);
				continue;
			}
			item.select();
			if (mustNext) {
				if (bot.button("Next >").isEnabled()) {
					bot.button("Next >").click();
				} else {

					log.info("Wizard '" + le.getName()
							+ "' does not lead anywhere");
					continue;
				}
			}
			// retrieve all interesting labels, checkboxes on form
			Display.getDefault().syncExec(new Runnable() {

				private boolean isTextFieldLabel(String label) {
					return label.trim().endsWith(":")
							|| label.trim().endsWith("*");
				}

				@SuppressWarnings("unchecked")
				public void run() {
					List<Label> labels = bot.getFinder().findControls(
							WidgetMatcherFactory.widgetOfType(Label.class));
					int i=0;
					for (Label l : labels) {
						if (i==0) {
							i++;
							continue;
						}
						String text = l.getText().replaceAll("\\&", "");
						if (!"".equals(text) && isTextFieldLabel(text)) {
							le.getTextFields().add(text.trim());
						}
					}
					List<Widget> chboxes = bot.getFinder().findControls(
							WidgetMatcherFactory.allOf(WidgetMatcherFactory
									.withStyle(SWT.CHECK, "SWT.CHECK"),
									WidgetMatcherFactory
											.widgetOfType(Button.class)));
					for (Widget l : chboxes) {
						String text = ((Button) l).getText().replaceAll("\\&",
								"");
						if (!"".equals(text)) {
							le.getChbFields().add(text);
						}
					}
				}
			});
			if (mustNext) {
				bot.button("< Back").click();
			}
		}
	}

	private void createEntity(Stack<SWTBotTreeItem> stack,
			List<LabelEntity> entities) {
		SWTBotTreeItem item = stack.peek();
		LabelEntity le = new LabelEntity();
		// log.info("Found entity "+item.getText());
		le.setName(item.getText());
		for (SWTBotTreeItem s : stack) {
			le.getPath().add(s.getText());
		}
		entities.add(le);
	}

	public void getItems(SWTBotTreeItem item, Stack<SWTBotTreeItem> stack,
			List<LabelEntity> entities, boolean leavesOnly) {
		stack.push(item);
		item.expand();
		SWTBotTreeItem[] items = item.getItems();
		if (items.length == 0) {
			// printStack(stack);
			createEntity(stack, entities);
		} else {
			for (SWTBotTreeItem i : items) {
				if (!leavesOnly) {
					createEntity(stack, entities);
				}
				getItems(i, stack, entities, leavesOnly);
			}
		}
		stack.pop();
	}

	public void getItemsAllNodes(SWTBotTreeItem item,
			Stack<SWTBotTreeItem> stack, List<LabelEntity> entities) {
		stack.push(item);
		item.expand();
		SWTBotTreeItem[] items = item.getItems();
		createEntity(stack, entities);
		if (items.length == 0) {
			// printStack(stack);

		} else {
			for (SWTBotTreeItem i : items) {
				getItems(i, stack, entities, true);
			}
		}
		stack.pop();
	}


	@BeforeClass
	public static void beforeClass() throws Exception {
		bot = new SWTWorkbenchBot();
		try {
			bot.viewByTitle("Welcome").close();
		} catch (WidgetNotFoundException ex) {

		}
		bot.sleep(3000);
	}
}
