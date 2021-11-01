JsOsaDAS1.001.00bplist00�Vscript_�// set things up
var app = Application.currentApplication();
app.includeStandardAdditions = true;
var notesApp = Application('Notes');
notesApp.includeStandardAdditions = true;

// choose which notes
var notes = notesApp.notes;
var whichNotes = app.chooseFromList(notes.name(), { withPrompt: "Which Notes?", multipleSelectionsAllowed: true });
// could proably just replace the whiceNotes variable with the names of the notes to be exported

if (whichNotes) {

	// choose save location
	var saveWhere = "/Users/jeremybutt/Desktop/Workout_data";
		
	if (saveWhere) {
	
		// loop through all notes
		for(var i=0; i<notes.length; i++) {
		
			// is this note one to be exported?
			if (whichNotes.indexOf(notes[i].name()) > -1) {
			
				//save file as html
				var filename = saveWhere+"/"+notes[i].name()+"_current"+".txt";
				var file = app.openForAccess(Path(filename), { writePermission: true });
				app.setEof(file, { to: 0 });
				app.write(notes[i].plaintext(), {to: file});
				app.closeAccess(file);
			}
		}
	}
}                              jscr  ��ޭ